#include <wx/wx.h>
#include <wx/treectrl.h>
#include <wx/infobar.h>
#include <stdint.h>
#include <wx/tglbtn.h>
#include <wx/stc/stc.h>
#include <wx/wupdlock.h>
#include "debug.hxx"
#include "dialogs.hxx"
#include "tydesc-factory.hxx"

#ifndef DEBUG_GUI_H
#define DEBUG_GUI_H


/*main init class, may add more members (such as panels or frames later
 *to shift initialization into main (most likely not needed though)*/
class debugApp : public wxApp{
public:
    virtual bool OnInit();
};

//FIXME: change the structure of the tree control classes to make member variables private.
class localTreeCtrl : public wxTreeCtrl{
public:
    localTreeCtrl(unsigned int strands, wxWindow* parent);
    int num_strands;
    wxTreeItemId root;
    wxTreeItemId strand;
    //every strand goes in here
    wxTreeItemId* strandArray;
};

class globalTreeCtrl : public wxTreeCtrl{
public:
    globalTreeCtrl(unsigned int strands, wxWindow* parent);
    int num_strands;
    wxTreeItemId root;
    wxTreeItemId global;
};

class controlToggles : public wxPanel{
public:
    controlToggles(debugger* parent);
    wxToggleButton* get_getToggle() { return this->getToggle; }
    wxToggleButton* get_setToggle() { return this->setToggle; }
    wxToggleButton* get_strandToggle() { return this->strandToggle; }
    wxToggleButton* get_stepToggle() { return this->stepToggle; }
private:
    debugger* debug;
    wxToggleButton* getToggle;
    wxToggleButton* setToggle;
    wxToggleButton* strandToggle;
    wxToggleButton* stepToggle;
    wxToggleButton* checkPtToggle;
};


//may be derived a wxWindow in the future, 
//but is not as these are represented as pop up dialogs
class controlPanels {
public:
    controlPanels(debugger* parent, wxString* items, unsigned int num_strands);
    ~controlPanels(); 
    setControlPanel* get_setControlPanel() { return this->sControlPanel; } 
    getControlPanel* get_getControlPanel() { return this->gControlPanel; } 
    strandControlPanel* get_strandControlPanel() { return this->strControlPanel; } 
    stepPanel* get_stepPanel() { return this->stpControlPanel; }

private:
    setControlPanel* sControlPanel;
    getControlPanel* gControlPanel;
    strandControlPanel* strControlPanel;
    stepPanel* stpControlPanel;
};

/*This is the main debugger class situated in a wxFrame.*/
class debugger : public wxFrame{
public:
    debugger();
    
    Diderot_world_t* get_world() { return this->wrld; }

    JSON::value* get_json_val() { return this->json_val; }

    DDebug::locals_function get_locals() { return this->locals; }
    DDebug::globals_function get_globals() { return this->globals; }
    DDebug::runtime_functions get_r_func() { return this->r_func; }

    /******************************************************************/

    controlPanels *get_controlPanels() { return this->cPanels; }
    initSetPanel* get_initSetPanel() { return this->isp; }

    /******************************************************************/

    globalTreeCtrl* get_globalCtrl() { return this->globalCtrl; }
    localTreeCtrl* get_localCtrl() { return this->localCtrl; }

    /******************************************************************/

    wxStatusBar* get_statusBar() { return this->statusBar; }

    /******************************************************************/

    void increment_time(int i) { time += i; }
    int get_time() { return time; }
    void set_time(unsigned int i) { time = i; }

    /******************************************************************/

    wxPanel* get_editorPanel() { return this->editorPanel; }
    wxPanel* get_infoPanel() { return this->infoPanel; }

    /******************************************************************/

    std::ostream* get_stdcerr() { return this->stdcerr; }

    /******************************************************************/

    wxTextCtrl* get_dialogBox() { return this->dialog_box; }


    void update_vars();
    void freeEverything();
    bool checkGlobalDefaults(DDebug::globals_function globals, wxString* items);
private:
    /*MAJOR FIXME: Many of these widgets can and will be modulized into new classes*/
    wxString filename;
    wxString programName;

    //menu bar
    wxMenuBar *mbar;
    //file tab
    wxMenu *file;
    wxMenu *controls;

    //interactive components
    wxStyledTextCtrl *textc;
    wxTextCtrl *dialog_box;
    //program output/messages and text editor widgets.
    wxPanel *editorPanel;
    //displays program state choices (decided by user) and values
    wxPanel *infoPanel;
    wxPanel *treePanel;
    wxFlexGridSizer *fgs1;
    wxBoxSizer* bsize;

    std::ostream* stdcerr;
    //Status bar currently shows time in super steps and strand information
    wxStatusBar* statusBar;

    wxToolBar *toolBar;
    //items is the array of variable names
    wxString* items;

    //tree control displays all variables
    globalTreeCtrl* globalCtrl;
    localTreeCtrl* localCtrl;
    //buttons to toggle get, set, strand, step, and checkpoint
    controlToggles* toggles;
    controlBook* notebook; 
    //Panels for the the widgets described in toggleButtons
    controlPanels* cPanels;
    initSetPanel* isp;

    //diderot components for debugger
    uint32_t time;
    Diderot_world_t *wrld;
    void* handle;
    JSON::value* json_val; 
    DDebug::locals_function locals;
    DDebug::globals_function globals;
    //most runtime functions loaded in here, will edit to include more later
    DDebug::runtime_functions r_func;

    void onCheck(wxCommandEvent &event);
    void onDebugInit(wxCommandEvent &event);
    void onChooseVariables(wxCommandEvent &event);
    void onOpen(wxCommandEvent &event);
    void onSave(wxCommandEvent &event);
    void onStep(wxCommandEvent &event);
    void onStrandID(wxCommandEvent &event);
    void onQuit(wxCloseEvent &event);
    void onSetToggle(wxCommandEvent &event);
    void onGetToggle(wxCommandEvent &event);
    void onStrandToggle(wxCommandEvent &event);
    void onStepToggle(wxCommandEvent &event);
    void onInitSetClose(wxCommandEvent &event);
};


#endif