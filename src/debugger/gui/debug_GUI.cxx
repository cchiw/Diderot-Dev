#include "debug.hxx"
#include "debug_GUI.hxx"

/**********************************READ***************************************/
/* MAJOR FIXME: There's a lot of redundancy in the code that possibly prevents compiler optimization
 * and increases runtime costs. Will try to optimize the code when I return */

/* MEDIUM FIXME: There are some memory leaks if one opens and initializes a new diderot program
 * when another is already initialized. Also to note: on shutdown, valgrind gives about 100 KB
 * of memory leaks. However, most of it has to do with the JSON parser and wxWidgets itself.
 * I therefore think that the 100 KB are just false-positives. */

/* MAJOR FIXME: Still need to add handling for images, strings, and sequences.
 * At this point, we don't have a proper implementation for representing those. */

/* MAJOR FIXME: Many of the private widgets can and will be modulized into new classes*/

/* MINOR FIXME:
        -Might have missed some this->...
        -Need to make some members private still */

/* ToDo :
    Debugger/Compiler Interface
    - Get_Size for strings, dynamic sequences
    - What to do about images
    - Checkpoints - Get and a Set:
        - Probably grabbing most the world information
    - Strand birth - new/die
        - how do we represent strand IDs?
        - communicating information about dynamic strands to debugger
*/


/*TODO: This GUI code is getting quite complicated. I will in the future attempt to create a tree representing window ownerships and dependencies.
    That should help with code modification and debugging*/

enum {
    ID_SAVE = 1,	// must start at 1 because macOS does not allow menu IDs == 0
    ID_DEBUG_INIT,
    ID_ON_CHECK,
    ID_STEP,
    ID_STRAND,
    ID_GET,
    ID_SET,
    ID_STEP_CHOICE,
};

/****************************HELPER FUNCTIONS**********************************/

/*  Constructs and returns array of wxString keys using the
 *  globals_functions and locals_functions maps.
 */
wxString* construct_key_array(DDebug::globals_function map_key_g,
    DDebug::locals_function map_key_l)
{
    int i;
    //size is the total size
    int size = (map_key_g.size()) + (map_key_l.size());
    wxString* arry = new wxString[size];
    /* create an iterator and iterate through the globals. Note that
     * the starting index is 0 for globals and the starting index for locals is the number of globals*/
    std::map<string, DDebug::global_var*>::iterator it;
    for(it = map_key_g.begin(), i = 0; it != map_key_g.end(); ++it, i++){
        *(arry + i) = wxString(it->first);
    }
    // create a second iterator and iterate through the locals
    std::map<string, DDebug::local_var*>::iterator it_1;
    for(it_1 = map_key_l.begin(); it_1 != map_key_l.end(); ++it_1, i++){
        *(arry + i) = wxString(it_1->first);
    }
    return arry;

}

/************************ debugger methods ************************************/

//constructor
debugger::debugger()
    : wxFrame(NULL, wxID_ANY, "Debugger", wxDefaultPosition, wxSize(900, 650)) {

    //interactive components
    this->textc = nullptr;
    this->dialog_box = nullptr;
    //program output/messages and text editor widgets.
    this->editorPanel = nullptr;
    //displays program state choices (decided by user) and values
    this->infoPanel = nullptr;
    this->treePanel = nullptr;
    this->fgs1 = nullptr;
    this->bsize = nullptr;

    //Status bar currently shows time in super steps and strand information
    this->statusBar = nullptr;

    this->toolBar = nullptr;
    //items is the array of variable names
    this->items = nullptr;

    //tree control displays all variables
    this->globalCtrl = nullptr;
    this->localCtrl = nullptr;
    //buttons to toggle get, set, strand, step, and checkpoint
    this->toggles = nullptr;
    this->notebook = nullptr;
    //Panels for the the widgets described in toggleButtons
    this->cPanels = nullptr;
    this->isp = nullptr;
    this->wrld = nullptr;
    this->json_val = nullptr;

    //ostream pointer points at cerr
    this->stdcerr = &cerr;

    //adds png support
    wxImage::AddHandler( new wxPNGHandler );
    //menu bar
    this->mbar = new wxMenuBar;
    //file tab
    this->file = new wxMenu;
    //control tab
    this->controls = new wxMenu;

    //The file tab is given the options: open and save
    this->file->Append(wxID_OPEN, "&Open");
    this->file->Append(ID_SAVE, "&Save\tCtrl-S");
    //the controls tab is given the step option
    this->controls->Append(ID_STEP_CHOICE, "&Step\tShift-S");
    //the tabs are then added to the menubar
    this->mbar->Append(this->file, wxT("&File"));
    this->mbar->Append(this->controls, wxString("&Controls"));
    this->SetMenuBar(this->mbar);

    //flex grid sizer arrays widgets in grid and allows for resizing
    this->fgs1 = new wxFlexGridSizer(2, 2, 0, 0);
    //panels to contain all widgets
    this->editorPanel = new wxPanel(this,
        wxID_ANY, wxPoint(0, 0), wxSize(900, 650));
    this->infoPanel = new wxPanel(this->editorPanel,
        wxID_ANY, wxPoint(600, 325), wxSize(300, 325));
    this->treePanel = new wxPanel(this->editorPanel,
        wxID_ANY, wxPoint(0, 325), wxSize(600, 325));
    this->editorPanel->SetAutoLayout(true);
    this->treePanel->SetBackgroundColour(*wxLIGHT_GREY);

    //Toolbar contains various runtime tools
    this->toolBar = CreateToolBar();
    wxBitmap init(wxT("init.png"), wxBITMAP_TYPE_PNG);
    wxBitmap step(wxT("step.png"), wxBITMAP_TYPE_PNG);
    wxBitmap checkmark(wxT("checkmark.png"), wxBITMAP_TYPE_PNG);
    this->toolBar->AddTool(ID_DEBUG_INIT,
        wxT("init"), init, wxString("initialize diderot world state"));
    this->toolBar->AddTool(ID_STEP,
        wxT("step"), step, wxString("step a given amount of super steps"));
    this->toolBar->Realize();

    //program information
    this->statusBar = CreateStatusBar(3);
    this->statusBar->PushStatusText(wxString("Time: 0"), 0);
    this->statusBar->PushStatusText(wxString("Alive Strands: 0"), 1);
    this->statusBar->PushStatusText(wxString("Stable Strands: 0"), 2);

    //toggle buttons for control panels
    //this->toggles = new controlToggles(this);
    /* Binding functions to events and IDs
     * the first enumeration is the wxWidgets event associated with the function
     * and the enumeration at the end of parameters is the ID associated with the specific
     * tool that emits that event. */
    Bind(wxEVT_MENU, &debugger::onOpen, this, wxID_OPEN);
    Bind(wxEVT_MENU, &debugger::onSave, this, ID_SAVE);
    Bind(wxEVT_TOOL, &debugger::onDebugInit, this, ID_DEBUG_INIT);
    Bind(wxEVT_TOOL, &debugger::onStep, this, ID_STEP);
    Bind(wxEVT_TOGGLEBUTTON, &debugger::onGetToggle, this, ID_GET);
    Bind(wxEVT_TOGGLEBUTTON, &debugger::onSetToggle, this, ID_SET);
    Bind(wxEVT_TOGGLEBUTTON, &debugger::onStrandToggle, this, ID_STRAND);
    Bind(wxEVT_TOGGLEBUTTON, &debugger::onStepToggle, this, ID_STEP);
    Bind(wxEVT_CLOSE_WINDOW, &debugger::onQuit, this);


    /* text controls: textc is the text editor and
     * dialog_box is where internal information is displayed. */
    this->textc = new wxStyledTextCtrl(this->editorPanel, -1, wxPoint(-1, -1),
        wxSize(600, 325));
    this->dialog_box = new wxTextCtrl(this->editorPanel, -1, wxT(""), wxPoint(600, 0),
        wxSize(300, 325), wxTE_MULTILINE | wxHSCROLL);

    /* flex gridsizer is initialized, which
     * allows for the widgets to be resized along with the parent window*/
    this->fgs1->AddGrowableRow(0,1);
    this->fgs1->AddGrowableRow(1,1);
    this->fgs1->AddGrowableCol(0,1);
    this->fgs1->AddGrowableCol(1,1);

    this->fgs1->Add(this->textc, 1, wxEXPAND);
    this->fgs1->Add(this->dialog_box, 1, wxEXPAND);
    this->fgs1->Add(this->treePanel, 1, wxEXPAND);
    this->fgs1->Add(this->infoPanel, 1, wxEXPAND);
    //this->fgs1->Add(this->toggles, 1, wxEXPAND);
    //this->fgs1->Add(this->notebook, 1, wxEXPAND);
    //bsize holds the two tree ctrls and allows for it to be fitted in the fgs
    this->bsize = new wxBoxSizer(wxHORIZONTAL);

    //The panels' sizers are set
    this->treePanel->SetSizer(this->bsize);
    this->editorPanel->SetSizer(this->fgs1);

    this->Center();

}

/* deletes everything if init is pressed again, aim is to prevent memory leak
 * FIXME: still some memory leakage - need to add deletions*/
void debugger::freeEverything(){
    if(this->cPanels){
        delete this->cPanels;
        this->cPanels = nullptr;
    }
    if(this->json_val){
        delete this->json_val;
        this->json_val = nullptr;
    }
    if(this->wrld){
        this->r_func.shutdown(this->wrld);
        this->wrld = nullptr;
    }
    if(this->bsize){
        this->bsize->Clear();
    }
    if(this->fgs1){
        this->fgs1->Clear();
    }
}


/* Function tied to the init button press event.
 * Loads and initializes almost all components of
 * the debugger and diderot program. */

void debugger::onDebugInit(wxCommandEvent &event){
    //resets everything if the init button was already pressed
    this->freeEverything();
    this->locals.clear();
    this->globals.clear();
    this->time = 0;
    if(this->localCtrl)
        this->localCtrl->DeleteChildren(this->localCtrl->strand);
    //checks the file extension for the necessary ".diderot"
    wxString extension = this->filename.AfterLast(wxUniChar('.'));
    if(!extension.Cmp(wxString("diderot"))){
        //finds the program name. ex: heron.diderot becomes heron
        int found = this->filename.find_last_of('.');
        int last_slash_index = this->filename.find_last_of('/');
        this->programName = this->filename.substr(0, found);
        //Making path a local variable for now 
        wxString path = this->filename.substr(0, last_slash_index);
        chdir(path.c_str());
        //initializes the dynamic library handle from the .so file
        this->handle =
            dlopen((this->programName.ToStdString() + ".so").c_str(), RTLD_NOW);
        //initializes the json value from the json file
        this->json_val = JSON::parse_file(this->programName.ToStdString() + ".json");
        if((!this->handle) || (!this->json_val)){
            cerr << "Invalid or nonexistant .so or .json file" << "\n";
            //error to be defined here if no .so or .json
        }
        else if(json_val){//if there's a json, then load all the functions in
            DDebug::load_local_functions(this->handle, this->json_val, &this->locals);
            DDebug::load_global_functions(this->handle, this->json_val, &this->globals);
            DDebug::load_runtime_functions(this->handle, this->json_val, &this->r_func);
            //bool result = true;
            //redirects everything that is outputted through stderr to the dialog_box
            wxStreamToTextRedirector redirect(this->dialog_box, this->stdcerr);
            {
                cerr << "Initializing... \n";
                //diderot world is allocated and initialized
                this->wrld = this->r_func.new_world();
                this->r_func.init_world(this->wrld);
                //items is an array that contains all the local and global variable keys
                this->items = construct_key_array(this->globals, this->locals);
                /* isp is displayed to the user before strands are allocated.
                 * This allows the user to modify the initial values or
                 * set values for uninitialized globals */
                this->isp = new initSetPanel(this,
                    this->items, wxSize(500, 300));
                /* This Bind is a little different as it Binds a debugger function to a button ID that
                 * is in the initialization set panel. It does not Bind to a component of this class.
                 * This is because on the destruction of isp the world is done initializing,
                 * allowing four world control panels and two tree controls to be initialized. */
                this->isp->Bind(wxEVT_BUTTON, &debugger::onInitSetClose, this, ID_OK);
                
                this->fgs1->Add(this->textc, 1, wxEXPAND);
                this->fgs1->Add(this->dialog_box, 1, wxEXPAND);
                this->fgs1->Add(this->treePanel, 1, wxEXPAND);
                this->fgs1->Add(this->infoPanel, 1, wxEXPAND);

                this->isp->Centre();
                cerr << "Diderot World Initialized\n";
            }
        }
    }
    else{
        this->textc->Clear();
    }
}

//handles what happens when the init panel is closed
void debugger::onInitSetClose(wxCommandEvent &event){
    //closes the initialization set panel
    this->isp->Destroy();
    //finishes the rest of world initialization
    this->r_func.set_verbose(this->wrld, 1);
    this->r_func.create_strands(this->wrld);
    unsigned int num_strands = this->r_func.num_strands(this->wrld);

    //new tree controls initialized
    this->localCtrl = new localTreeCtrl(num_strands,
        this->treePanel);
    this->globalCtrl = new globalTreeCtrl(num_strands,
        this->treePanel);
    //the control panels are initialized
    //cPanels = new controlPanels(this, this->items, num_strands);
    this->notebook = new controlBook(this, this->items, num_strands);
    //this->fgs1->Add(this->notebook, 1, wxEXPAND);

    this->bsize->Add(this->globalCtrl, 1, wxEXPAND);
    this->bsize->Add(this->localCtrl, 1, wxEXPAND);
    //the status bar is updated to reflect the number of strands
    this->statusBar->SetStatusText(
        wxString("Alive Strands: " + to_string(
            this->r_func.num_active_strands(this->wrld))), 1);
    this->statusBar->SetStatusText(
        wxString("Stable Strands: " + to_string(
            this->r_func.num_stable_strands(this->wrld))), 2);
    this->update_vars();

}
/*  Takes the diderot program's JSON file and gets the value for
 *  each local or global variable the user wants to view.
 *  FIXME: getting dynamic sequences, strings, and images will need to be handled
 *  (most likely will not be able to just use addrof)
 */
void debugger::update_vars(){
    const DDebug::type_desc *tydesc;
    DDebug::value *val;
    DDebug::global_var* glob;
    DDebug::local_var* loc;
    wxTreeItemId* treeVal;
    //clears the tree controls' values before appending the updated values
    this->globalCtrl->DeleteChildren(this->globalCtrl->global);
    for(int l = 0; l < this->localCtrl->num_strands; l++){
        treeVal = this->localCtrl->strandArray;
        if(treeVal[l] != nullptr)
            this->localCtrl->DeleteChildren(treeVal[l]);
    }
    //wxStreamToTextRedirector redirect(dialog_box);
    //{
        int size = this->globals.size() + this->locals.size();
        //turns the json value initialized in the constructor into an object
        const JSON::object* jobj = this->json_val->asObject();
        wxCheckListBox* clBox = this->notebook->get_getControlPanel()->get_clgBox();
        //loops through every variable and checks if it's checked in the
        //getControlPanel's check list box.
        for (int i = 0; i < size; i++)
        {
            //if the check list box is checked at index i, then we display
            if(clBox->IsChecked(i)){
                /* if i < the number of globals, then we are dealing with a global variable
                 * else, a local. */
                if(i < this->globals.size()){
                    //finds the inputs array in the JSON.
                    JSON::value* jval = (*jobj)[string("inputs")];
                    const JSON::array* in_arry = jval->asArray();
                    //loops through the input array and searches for the correct input
                    for(int j = 0; j < this->globals.size(); j++){
                        string var_name = (*(*in_arry)[j]
                            ->asObject())[string("name")]->asString()->val();
                        if(!var_name.compare(clBox->GetString(i).ToStdString())){
                            //access the value of the global and then use the get function on it
                            glob = this->globals.find(var_name)->second;
                            val = glob->val;
                            glob->get(wrld, val->addrof());
                            //then append the variable's value to the globals tree control.
                            this->globalCtrl->AppendItem(this->globalCtrl->global,
                                wxString(var_name
                                    + ":" + glob->tydesc->to_string() + ": "
                                    + val->to_string()));
                        }
                    }
                }
                else{
                    //finds the strand array in the JSON
                    JSON::value* jval = (*jobj)[string("strand")];
                    const JSON::object* strand_obj = jval->asObject();
                    //loops through the strand array and searches for the correct variable
                    for(int j = 0; j < this->locals.size(); j++){
                        string var_name = (*((*(*strand_obj)
                            [string("state-vars")]->asArray())[j]->asObject()))
                            [string("name")]->asString()->val();
                        if(!var_name.compare(this->notebook->get_getControlPanel()->get_clgBox()
                            ->GetString(i).ToStdString())){
                            loc = this->locals.find(var_name)->second;
                            /* This loop goes through the strand selected array in strand handler.
                             * If the strand is selected, then the variable's value will be appended
                             * to its strand number in the local tree control */
                            cout << localCtrl->num_strands << "\n";
                            for(int k = 0; k < this->localCtrl->num_strands; k++){
                                if(this->notebook->get_strandControlPanel()->get_sHandler()
                                    ->get_isStrandSelected()[k]){
                                    val = loc->val;
                                    memcpy(val->addrof(), loc->get(wrld, k),
                                        loc->tydesc->size_in_bytes());
                                    this->localCtrl->AppendItem(this->localCtrl->strandArray[k],
                                        wxString(var_name + ":" +
                                            loc->tydesc->to_string() + ": "
                                            + val->to_string()));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

//}


//enables the checked variables in the variableTreeCtrl
void debugger::onCheck(wxCommandEvent &event){
    this->dialog_box->AppendText(wxString("---------------------\n"));
    this->update_vars();
}

//save the file
void debugger::onSave(wxCommandEvent &event){
    if((this->textc != nullptr) && (this->filename.compare(""))){
        this->textc->SaveFile(filename);
    }
}

//open a file into the editor dialog
void debugger::onOpen(wxCommandEvent &event){
    wxFileDialog fileDialog(this);
    if (fileDialog.ShowModal() == wxID_OK){
      wxString fileName(fileDialog.GetPath());
      this->filename = fileName;
      this->textc->LoadFile(fileName);
    }
}

//closes all open windows and then closes the parent window
void debugger::onQuit(wxCloseEvent &event){
    if(this->cPanels)
        delete this->cPanels;
    if(this->json_val)
        delete this->json_val;
    this->Destroy();
}

/* For the single step button in the toolbar.
 * Steps forward by 1 superstep and updates the status bar */
void debugger::onStep(wxCommandEvent &event){
    wxStreamToTextRedirector redirect(this->dialog_box, this->stdcerr);
    {
        uint32_t num_steps;
        if(wrld != nullptr){
            num_steps = this->r_func.run(this->wrld, 1);
            if(num_steps < 1){
                cerr << "Execution finished\n";
                return;
            }
            this->update_vars();
            this->time++;
            this->statusBar->SetStatusText(wxString(
                "Time: " + to_string(this->time)), 0);
            this->statusBar->SetStatusText(
                wxString("Alive Strands: " + to_string(
                    this->r_func.num_active_strands(this->wrld))), 1);
            this->statusBar->SetStatusText(
                wxString("Stable Strands: " + to_string(
                    this->r_func.num_stable_strands(this->wrld))), 2);
        }
    }

}

/* The next four functions handle what to do if
 * their respective buttons are pressed. i.e.
 * 1. If the control panels are not allocated, then the buttons
 *    cannot be pressed.
 * 2. If the button is pressed and passes that inital check, then the
 *    panel is displayed.
 * 3. If the button is unpressed, the panel is hidden.
 * 4. Anything else constitutes the button being unpressed */

void debugger::onGetToggle(wxCommandEvent &event){
    wxToggleButton* toggleButton = this->toggles->get_getToggle();
    if(this->cPanels == nullptr){
        toggleButton->SetValue(false);
        return;
    }
    getControlPanel* panel_var = this->cPanels->get_getControlPanel();
    if(!toggleButton->GetValue())
        panel_var->Show(false);
    else if(panel_var != nullptr)
        panel_var->Show(true);
    else
        toggleButton->SetValue(false);
}

void debugger::onSetToggle(wxCommandEvent &event){
    wxToggleButton* toggleButton = this->toggles->get_setToggle();
    if(this->cPanels == nullptr){
        toggleButton->SetValue(false);
        return;
    }
    setControlPanel* panel_var = this->cPanels->get_setControlPanel();
    if(!toggleButton->GetValue())
        panel_var->Show(false);
    else if(panel_var != nullptr)
        panel_var->Show(true);
    else
        toggleButton->SetValue(false);
}


void debugger::onStrandToggle(wxCommandEvent &event){
    wxToggleButton* toggleButton = this->toggles->get_strandToggle();
    if(this->cPanels == nullptr){
        toggleButton->SetValue(false);
        return;
    }
    strandControlPanel* panel_var = this->cPanels->get_strandControlPanel();
    if(!toggleButton->GetValue())
        panel_var->Show(false);
    else if(panel_var != nullptr)
        panel_var->Show(true);
    else
        toggleButton->SetValue(false);
}

void debugger::onStepToggle(wxCommandEvent &event){
    wxToggleButton* toggleButton = this->toggles->get_stepToggle();
    if(this->cPanels == nullptr){
        toggleButton->SetValue(false);
        return;
    }
    stepPanel* panel_var = this->cPanels->get_stepPanel();
    if(!toggleButton->GetValue())
        panel_var->Show(false);
    else if(panel_var != nullptr)
        panel_var->Show(true);
    else
        toggleButton->SetValue(false);
}

/*****************************variableTreeCtrl methods**************************/

globalTreeCtrl::globalTreeCtrl(unsigned int strands, wxWindow* parent) :
    wxTreeCtrl(parent, wxID_ANY, wxPoint(0, 0), wxSize(300, 325)){
    this->num_strands = strands;
    //initial state of variable tree
    this->root = this->AddRoot(wxString("Variables"));
    this->global = this->AppendItem(root, wxString("Globals"));
}

localTreeCtrl::localTreeCtrl(unsigned int strands, wxWindow* parent) :
    wxTreeCtrl(parent, wxID_ANY, wxPoint(300, 0), wxSize(300, 325)){
    this->num_strands = strands;
    //initial state of variable tree
    this->root = this->AddRoot(wxString("Variables"));
    //strand array holds all the strands
    this->strandArray = new wxTreeItemId[num_strands];
    this->strand = this->AppendItem(root, wxString("Strands"));
}


/*********************controlPanel methods****************************/
//the four world control panels are allocated and initialized here
controlPanels::controlPanels(debugger* parent, wxString* items, unsigned int num_strands){
    //this->strControlPanel = new strandControlPanel(parent,
    //    num_strands);
    //REMOVE LATER, JUST AS A FILLEr
    //controlBook* filler = NULL;
    //this->gControlPanel = new getControlPanel(parent, filler, items);
    //this->gControlPanel->SetBackgroundColour(*wxLIGHT_GREY);
    //this->gControlPanel->Show(false);
    this->sControlPanel = new setControlPanel(parent, items, wxSize(300, 200));
    this->sControlPanel->Show(false);
    //this->stpControlPanel = new stepPanel(parent);
    //this->stpControlPanel->Show(false);
}

controlPanels::~controlPanels(){
    if(this->gControlPanel)
        this->gControlPanel->Close(true);
    if(this->sControlPanel)
        this->sControlPanel->Close(true);
    if(this->stpControlPanel)
        this->stpControlPanel->Close(true);
    if(this->strControlPanel)
        this->strControlPanel->Close(true);
}


/*****************controlToggles methods*******************************/
//the four toggle buttons are allocated and initialized here.
controlToggles::controlToggles(debugger* parent) : wxPanel(parent->get_editorPanel(),
        wxID_ANY, wxPoint(600, 325), wxSize(300, 325)){
    debug = parent;
    this->getToggle = new wxToggleButton(this,
        ID_GET, wxString("Get variables"), wxPoint(30, 10), wxSize(100, 40));
    this->setToggle = new wxToggleButton(this,
        ID_SET, wxString("Set variables"), wxPoint(30, 60), wxSize(100, 40));
    this->strandToggle = new wxToggleButton(this,
        ID_STRAND, wxString("Strands"), wxPoint(30, 110), wxSize(100, 40));
    this->stepToggle = new wxToggleButton(this,
        ID_STEP, wxString("Stepping"), wxPoint(30, 160), wxSize(100, 40));
}
