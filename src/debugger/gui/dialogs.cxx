#include "dialogs.hxx"
#include "debug_GUI.hxx"
#include <wx/tokenzr.h>

/*FIXME: STILL NEEDS SOME OPTIMIZATION*/



/***********************strandHandler methods*********************************/
//constructor that sets everything in 
//the bool array to false (because no strands are selected yet!).
strandHandler::strandHandler(unsigned int strands, wxWindow* parent) : 
    wxTextCtrl(parent, ID_STRAND_INPUT, wxT(""), 
        wxDefaultPosition, wxSize(150,-1), wxHSCROLL){
    this->num_strands = strands;
    this->isStrandSelected = new bool[strands];
    for(int i = 0; i < num_strands; i++){
        this->isStrandSelected[i] = false;
    }
}

strandHandler::~strandHandler(){
    if(this->isStrandSelected)
        delete [] this->isStrandSelected;
}
/*Parses the range given in the strand_input textCtrl
 *and enables the strands by changing the value to true*/
bool strandHandler::parse_string(wxString str){
    int strands = this->num_strands;
    if(!str.Cmp("")){
        
        for(int i = 0; i < strands; i++){
            this->isStrandSelected[i] = true;
        }
        return 1;
    }
    //wxStreamToTextRedirector redirect(this->debug->dialog_box);
    //{
        //the wxString tokenizer tokenizes by spaces and commas
        wxStringTokenizer tokenizer(str, ", ");
        while(tokenizer.HasMoreTokens()){
            //one token is the equivalent of a strand range
            wxString token = tokenizer.GetNextToken();
            /* If there is a dash, then the range is multiple strands. 
             * Otherwise, it is a single strand. */
            if(token.Contains(wxString("-")) && (!(token[0] == '-'))){
                //second tokenizer removes the dashes and extracts the numbers
                wxStringTokenizer tokenizer2(token, "-");
                //counter decides which number is the first number and which is the second
                int first, second, counter = 0;
                while(tokenizer2.HasMoreTokens() && (counter != 2)){
                    wxString temp_int = tokenizer2.GetNextToken();
                    if(counter == 0)
                        first = wxAtoi(temp_int);
                    else
                        second = wxAtoi(temp_int);
                    counter++;
                }
                //checks for any improper inputs
                if((first >= strands) || (second >= strands) 
                    || (first < 0) || (second < 0) || (second < first)){
                    cout << first << " " << second << " out of bounds or invalid ordering of range\n";
                    return 0;
                }
                //sets the strands in the range to be true, or selected
                for(int j = first; j <= second; j++)
                    this->isStrandSelected[j] = true;
            }
            else{
                this->isStrandSelected[wxAtoi(token)] = true;
            }
        } 
        return 1;
    //}
}
/******************************controlBook methods*******************************/
controlBook::controlBook(debugger* debug, wxString* items, int num_strands) : 
    wxNotebook(debug->get_infoPanel(), wxID_ANY, wxPoint(0, 0), wxSize(300, 325)){
    this->debug = debug;
    //cout << this <<"\n";
    this->gControlPanel = new getControlPanel(debug, this, items);
    this->gControlPanel->SetBackgroundColour(*wxLIGHT_GREY);
    this->strControlPanel = new strandControlPanel(debug, this, num_strands);
    this->stpControlPanel = new stepPanel(debug, this);
    this->AddPage(this->gControlPanel, "Get", true);
    this->AddPage(this->strControlPanel, "Strand", true);
    this->AddPage(this->stpControlPanel, "Step", true);


}
/**************************getControlPanel methods***************************/

/* getControlPanel displays a check list box that lets the user
 * decide which variables and their values to display */ 
getControlPanel::getControlPanel(debugger* parent, controlBook* bookParent, wxString* items) : 
    wxPanel(bookParent, wxID_ANY, wxDefaultPosition){
    this->debug = parent;
    DDebug::globals_function g = this->debug->get_globals();
    DDebug::locals_function l = this->debug->get_locals();
    unsigned int size = g.size() + l.size();
    //check list box
    this->clgBox = new wxCheckListBox(this, wxID_ANY, wxDefaultPosition, 
                wxSize(300, 200), size, items);
    this->clgBox->SetBackgroundColour(*wxLIGHT_GREY);
    Bind(wxEVT_CHECKLISTBOX, &getControlPanel::onCheck, this);
    //this->EnableCloseButton(false);
    DDebug::value* valu;
    //checks every global element of the checklist box to display all globals initially
    for(int i = 0; i < g.size(); i++){
        valu = g.find(items[i].ToStdString())->second->val;
        //images and strings not implemented yet
        if(dynamic_cast<DDebug::image_value*>(valu) || dynamic_cast<DDebug::string_value*>(valu))
            continue;
        this->clgBox->Check(i);
    }
}

void getControlPanel::onCheck(wxCommandEvent &event){
    //debug->dialog_box->Clear();
    this->debug->update_vars();
}

getControlPanel::~getControlPanel(){
    if(this->clgBox){
        delete this->clgBox;
    }
}
/**************************setControlPanel methods***************************/

/* Set control panel allows the user to set the value of global variables.
 * Local variables feature might be added*/ 
setControlPanel::setControlPanel(debugger* parent, wxString* items, wxSize size) :
    wxFrame(NULL, wxID_ANY, wxString("Set Dialog"), wxDefaultPosition, size){
    this->varList = new wxComboBox(this, 
        ID_VARLIST, wxEmptyString, wxPoint(5, 5), wxSize(90, 30), 8, items);
    wxButton *setButton = new wxButton(this, 
        ID_SETBUTTON, wxString("SET"), wxPoint(195, 5), wxSize(90, 30));
    Bind(wxEVT_BUTTON, &setControlPanel::onSetButton, this, ID_SETBUTTON);
    this->itms = items;
    setValueCtrl = new wxTextCtrl(this, 
        -1, wxT(""), wxPoint(5, 80), wxSize(160, 30));
    Bind(wxEVT_COMBOBOX, &setControlPanel::onVarSelect, this, ID_VARLIST);
    this->debug = parent;
    this->EnableCloseButton(false);
}

setControlPanel::~setControlPanel(){
    if(this->varList)
        delete this->varList;
    if(this->setValueCtrl)
        delete this->setValueCtrl;

}

void setControlPanel::onVarSelect(wxCommandEvent &event){
    //cout << debug << "\n";
    if((this->selected_var = this->varList->GetSelection()) == wxNOT_FOUND){
        //strandList->Enable(false);
    }
    if(this->selected_var >= this->debug->get_globals().size()){
        cout << "local\n";
        //strandList->Enable(true);
    }
    else{
        //strandList->Enable(false);
    }

}

/* Function that is executed when the set button is pressed.
 * Sets the value of that variable by checking the type and using its
 * the set function */
void setControlPanel::onSetButton(wxCommandEvent &event){
    bool success = false;
    if(this->selected_var == wxNOT_FOUND)
        ; // error message
    else if((this->selected_var >= this->debug->get_globals().size()) && 
        (this->selected_strand != wxNOT_FOUND)){
        ; //should set local val here
    }
    else{
        DDebug::value* valu;
        DDebug::global_var* g_var = this->debug->get_globals().find(
            this->varList->GetString(this->selected_var).ToStdString())->second;
        Diderot_world_t* world = this->debug->get_world();
        wxString val_to_set = this->setValueCtrl->GetValue();
        valu = g_var->tydesc->from_string(val_to_set.ToStdString());
        if(DDebug::bool_value* v = dynamic_cast<DDebug::bool_value*>(valu)){
            success = g_var->set_value(world, v->valueof());
        }
        else if(DDebug::string_value *v =
            dynamic_cast<DDebug::string_value*>(valu)){
            success = g_var->set_value(world, v->valueof());
        }
        else if(DDebug::int32_value* v = 
            dynamic_cast<DDebug::int32_value*>(valu)){
            success = g_var->set_value(world, v->valueof()); 
        }
        else if(DDebug::int64_value* v = 
            dynamic_cast<DDebug::int64_value*>(valu)){
            success = g_var->set_value(world, v->valueof()); 
        }
        else if(DDebug::tensor32_value* v = 
            dynamic_cast<DDebug::tensor32_value*>(valu)){
            if(v->size_in_bytes() == 4)
                success = g_var->set_value(world, *(float*)v->addrof());
            else
                success = g_var->set_value(world, (float*)v->addrof()); 
        }
        else if(DDebug::tensor64_value* v = 
            dynamic_cast<DDebug::tensor64_value*>(valu)){
            if(v->size_in_bytes() == 4)
                success = g_var->set_value(world, *(double*)v->addrof());
            else
                success = g_var->set_value(world, (double*)v->addrof());  
        }
        else if(DDebug::image_value* v = 
            dynamic_cast<DDebug::image_value*>(valu)){
            //success = g_var->set_value(world, v->valueof().c_str()); 
        }
        //insert dynamic sequence and regular sequences
        if((this->debug->get_localCtrl()) && (this->debug->get_controlPanels())){
            this->debug->update_vars();
        }
        else{
            //FIXME: Need to implement an update method for the initial panel
        }
    }
}

/***************************initSetPanel methods********************************/
initSetPanel::initSetPanel(debugger* parent, wxString *items, wxSize size) : 
    setControlPanel(parent, items, size) {
    okButton = new wxButton(this, 
        ID_OK, wxString("OK"), wxPoint(5, 150), wxSize(90, 30));
    //Bind(wxEVT_BUTTON, &initSetPanel::onOk, this, ID_OK);
    this->Show(true);
    lbox = new wxListBox(this, wxID_ANY, wxPoint(200, 50), wxSize(200, 240), 
        this->get_debug()->get_globals().size(), this->checkGlobals());
}

initSetPanel::~initSetPanel(){
    if(okButton)
        delete okButton;
}

/* Checks if the global variables have defaults.
 * This is needed as ones with no defaults have no get function
 * Also returns the array of global variables and their values */
wxString* initSetPanel::checkGlobals(){
    std::map<string, DDebug::global_var*>::iterator it;
    DDebug::globals_function glob = this->get_debug()->get_globals();
    wxString* global_list = new wxString[glob.size()];
    wxString temp;
    Diderot_world_t* world = this->get_debug()->get_world();
    
    int i;
    //FIXME: HANDLE STRINGS AND IMAGES
    DDebug::global_var *g_temp;
    for(it = glob.begin(), i = 0; it != glob.end(); ++it, i++){
        g_temp = it->second;
        DDebug::value* valu = g_temp->val;
        if(dynamic_cast<DDebug::image_value*>(valu) || dynamic_cast<DDebug::string_value*>(valu)){
            temp = wxString(it->first + ": NOT IMPLEMENTED" );
            //cout << g_temp->set_value(world, "cubic.nrrd") << "\n"; 
        }
        else if(g_temp->definedDefault){
            g_temp->get(world, g_temp->val->addrof());
            temp = wxString(it->first + ": " + g_temp->val->to_string());
        }
        else{
            temp = wxString(it->first + ": uninitialized (WARNING: you may set one or ignore this message)");
        }
        global_list[i] = temp;
    }
    return global_list;
}

/**************************strandControlPanel methods**************************/
//allocates the strand panel and binds its button to the appropriate function
strandControlPanel::strandControlPanel(debugger* parent, controlBook* bookParent, unsigned int strands) :
    wxPanel(bookParent, wxID_ANY, wxDefaultPosition){
    sHandler = new strandHandler(strands, this);
    debug = parent;
    //this->EnableCloseButton(false);
    button = new wxButton(this, ID_STRANDID, 
        wxString("STRAND"), wxPoint(200, 5), wxSize(90, 30));
    Bind(wxEVT_BUTTON, &strandControlPanel::onStrandID, this, ID_STRANDID);
}

strandControlPanel::~strandControlPanel(){
    if(sHandler)
        delete sHandler;
    if(button)
        delete button;
}

/* When the strand ranges are inputted and confirmed, this is called to append the strands
 * to the local tree control in debugger*/
void strandControlPanel::onStrandID(wxCommandEvent &event){ 
    debugger* dbg = this->debug;
    localTreeCtrl* lCtrl = dbg->get_localCtrl();
    lCtrl->DeleteChildren(dbg->get_localCtrl()->strand);
    
    strandHandler* sHndler = this->get_sHandler();
    //gets the selection array from the strandHandler
    bool* arry = sHndler->get_isStrandSelected();
    //resets the array
    for(int i = 0; i < lCtrl->num_strands; i++){
        arry[i] = false;
    }
    
    //parses the strand range string and gives the new selected values 
    sHndler->parse_string(sHndler->GetValue());
      
    //loops through the selection array and appends all strands selected
    for(int j = 0; j < lCtrl->num_strands; j++){                
        if(arry[j])
            lCtrl->strandArray[j] = 
                lCtrl->AppendItem(lCtrl->strand, 
                    wxString("strand") + " " + wxString(std::to_string(j)));
    }
    dbg->update_vars();
}

/************************StepControlPanel***********************************************/

//a panel in which to control the running of the program
stepPanel::stepPanel(debugger* parent, controlBook* parentNotebook) : wxPanel(parentNotebook, wxID_ANY, wxDefaultPosition, wxSize(300, 200)){
    this->debug = parent;
    //this->EnableCloseButton(false);
    this->step = new wxButton(this, 
        ID_STEP_BUTTON, wxString("Step"), wxPoint(200, 5), wxSize(90, 30));
    this->input = new wxTextCtrl(this, 
        wxID_ANY, wxT(""), wxDefaultPosition, wxSize(150,-1), wxHSCROLL);
    this->confirmButton = new wxToggleButton(this, 
        ID_CONFIRM, wxString("confirm"), wxPoint(200, 45), wxSize(90, 30));
    

    Bind(wxEVT_BUTTON, &stepPanel::onStep, this, ID_STEP_BUTTON);
    Bind(wxEVT_TOGGLEBUTTON, &stepPanel::onConfirm, this, ID_CONFIRM);
}


stepPanel::~stepPanel(){
    if(this->step)
        delete this->step;
    if(input)
        delete this->input;
    if(confirmButton)
        delete this->confirmButton;
}

void stepPanel::onConfirm(wxCommandEvent &event){
    this->input->Enable(!(this->confirmButton->GetValue()));
}

//similar to debugger::onStep. However, the addition is the ability to specify num steps
void stepPanel::onStep(wxCommandEvent &event){
    wxStreamToTextRedirector redirect(this->debug->get_dialogBox(), this->debug->get_stdcerr());
    {
        std::string s_value = this->input->GetValue().ToStdString();
        Diderot_world_t* world = this->debug->get_world();
        wxStatusBar* sbar = this->debug->get_statusBar();
        DDebug::runtime_functions rf = this->debug->get_r_func();
        uint32_t num_steps;
        try
        {
            if(world == nullptr){
                return;
            }
            //if there is no input, step of 1 is implied
            if(s_value.compare("") == 0){
                num_steps = rf.run(world, 1);
                if(num_steps < 1){
                    cerr << "Execution finished\n";
                    return;
                }
                this->debug->update_vars();
                this->debug->increment_time(1);       
            }
            else{
                unsigned int steps_requested = stoi(s_value);
                num_steps = rf.run(world, steps_requested);
                if(num_steps < steps_requested){
                    cerr << "Execution finished\n";
                    return;
                }
                this->debug->update_vars();
                this->debug->increment_time(steps_requested); 
            }
            sbar->SetStatusText(wxString(
                "Time: " + to_string(debug->get_time())), 0);
            sbar->SetStatusText(
                wxString("Alive Strands: " + to_string(
                    rf.num_active_strands(world))), 1);
            sbar->SetStatusText(
                wxString("Stable Strands: " + to_string(
                    rf.num_stable_strands(world))), 2);
        }
        catch(std::invalid_argument& e)
        {
            cerr << "invalid argument\n";
            return;
        }
        catch(std::out_of_range& e)
        {
            cerr << "out_of_range\n";
            return;
        }
    }
}