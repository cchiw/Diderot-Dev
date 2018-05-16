/*! \file main.cxx
 *
 * \author John Reppy
 */

#include "debug_GUI.hxx"
/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

wxIMPLEMENT_APP(debugApp);

bool debugApp::OnInit(){
    debugger* debug = new debugger();
    debug->Show(true);
    return true;
}

