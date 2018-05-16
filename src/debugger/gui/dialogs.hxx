#include <wx/wx.h>
#include <wx/tglbtn.h>
#include <wx/notebook.h>
#include "debug.hxx"
//#include "debug_GUI.hxx"

#ifndef DIALOGS_H
#define DIALOGS_H

enum{
    ID_STRAND_INPUT,
    ID_STRANDSELECT,
    ID_VARLIST,
    ID_SETBUTTON,
    ID_STRANDID,
    ID_OK,
    ID_STEP_BUTTON,
    ID_CONFIRM,
    ID_SET_PANEL
};

class debugger;

class controlBook;
//a text control that parses inputted strand ranges
class strandHandler : public wxTextCtrl{
public:
    strandHandler(unsigned int strands, wxWindow* parent);
    ~strandHandler();
    bool* get_isStrandSelected() { return this->isStrandSelected; }
    bool parse_string(wxString str);
private:
    int num_strands;
    bool* isStrandSelected;
};

class strandControlPanel : public wxPanel{
public: 
    strandControlPanel(debugger* parent, controlBook* parentNotebook, unsigned int strands);
    ~strandControlPanel();
    debugger* get_debug() { return this->debug; }
    strandHandler* get_sHandler() { return this->sHandler; }
private:
    strandHandler* sHandler = nullptr;
    debugger *debug = nullptr;
    wxButton* button = nullptr;
    void onStrandID(wxCommandEvent &event);
};

class getControlPanel : public wxPanel{
public:
    ~getControlPanel();
    getControlPanel(debugger* parent, controlBook* parentNotebook, wxString* items);
    wxCheckListBox* get_clgBox() { return this->clgBox; }
    debugger* get_debug() { return this->debug; }
private:
    wxCheckListBox *clgBox = nullptr;
    debugger* debug = nullptr;
private:
    void onCheck(wxCommandEvent &event);
};

class setControlPanel : public wxFrame{
public:
    ~setControlPanel();
    setControlPanel(debugger* parent, wxString *items, wxSize size);
    debugger* get_debug() { return this->debug; }
private:
    debugger* debug = nullptr;
    wxString* itms;
    int selected_var = wxNOT_FOUND;
    int selected_strand = wxNOT_FOUND;
    wxComboBox* varList = nullptr;
    wxComboBox* strandList = nullptr;
    wxTextCtrl* setValueCtrl = nullptr;
    //only to be used in initial set
    bool isDone = false;
    void onQuit(wxCommandEvent &event);
    void onStrandSelect(wxCommandEvent &event);
    void onVarSelect(wxCommandEvent &event);
    void onSetSelect(wxCommandEvent &event);
    void onSetButton(wxCommandEvent &event);

};

class initSetPanel : public setControlPanel{
public:
    ~initSetPanel();
    initSetPanel(debugger* parent, wxString *items, wxSize size);
private:
    wxButton *okButton;
    wxListBox* lbox = nullptr;
    wxString* checkGlobals();
    void checkInitialized(vector<wxString> &uninitialized);
    void onOk(wxCommandEvent &event);
};

class stepPanel : public wxPanel{
public:
    ~stepPanel();
    stepPanel(debugger* parent, controlBook* parentNotebook);
    debugger* get_debug() { return this->debug; }
private:
    wxToggleButton *confirmButton = nullptr;
    wxButton *step = nullptr;
    wxTextCtrl* input = nullptr;
    debugger* debug = nullptr;
    void onConfirm(wxCommandEvent &event);
    void onStep(wxCommandEvent &event);
};

class controlBook : public wxNotebook{
public:
    controlBook(debugger* parent, wxString* items, int strands);
    getControlPanel* get_getControlPanel() { return this->gControlPanel; }
    strandControlPanel* get_strandControlPanel() { return this->strControlPanel; } 
    stepPanel* get_stepControlPanel() { return this->stpControlPanel; }
private:
    setControlPanel* sControlPanel;
    getControlPanel* gControlPanel;
    strandControlPanel* strControlPanel;
    stepPanel* stpControlPanel;
    debugger* debug;
};

#endif