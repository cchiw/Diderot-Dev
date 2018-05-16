#include "debug.hxx"


using namespace std;
bool isLongInt(JSON::value* json_value){
    const JSON::object* jobj = json_value->asObject();
    if(!string((*jobj)[string("int-size")]->asString()->val()).compare("32"))
        return false;
    else
        return true;
}

bool isDouble(JSON::value* json_value){
    const JSON::object* jobj = json_value->asObject();
    if(!string((*jobj)[string("float-size")]->asString()->val()).compare("32"))
        return false;
    else
        return true;
}

//NOTE: might just make these two functions a part of the class constructors
bool DDebug::load_global_functions(void* handle, JSON::value* json_value, 
    DDebug::globals_function* glob){
    /* Open JSON file here
     * Loop through all the get functions and load the information
     * into an array of globals_function structs
     * Note: Will use dlsym. Ret 0 if success, 1 if not*/

    DDebug::tydesc_factory *tyfact = nullptr;
    string set_symbol;
    if(!json_value){
        cout << "JSON value uninitialized.\n";
        return 1;
    }
    bool isLInt = isLongInt(json_value);
    bool isDbl = isDouble(json_value);
    string inputs = "inputs";
    string get = "get";
    string set = "set";
    string name = "name";
    string type = "type";
    string hasDef = "has-default";
    const JSON::object* jobj = json_value->asObject();
    JSON::value* jval = (*jobj)[inputs];
    const JSON::array* in_arry = jval->asArray();
    int length = in_arry->length();
    int i;
    for(i = 0; i < length; i++){
        if(tyfact)
            delete tyfact;
        tyfact = new DDebug::tydesc_factory(isLInt, isDbl);

        //the following section creates type descriptors, get symbols, set symbols to load into the struct
        const DDebug::type_desc* tyd = tyfact->mk_tydesc(*((*(*in_arry)[i]->asObject())[type]));
        
        string key = (((*(*in_arry)[i]->asObject())[name])->asString())->val();
        bool hasDefault = (((*(*in_arry)[i]->asObject())[hasDef])->asBool())->val();
        string get_symbol;
        if(hasDefault)
            get_symbol = (*(((*(*in_arry)[i]->asObject())[get])->asObject()))[name]->asString()->val();
        else
            continue;
        
        //FIXME: Some programs have two or more set functions for one global. 
        //"set" therefore is an array rather than an object. Need to check if set is array or not.
        //For now, I will take the first set function in the array which is the "set by ipos" funcs.
        JSON::value* set_field = ((*((*in_arry)[i]->asObject()))[set]);
        if(set_field->isArray()){
            set_symbol = (*(*(set_field->asArray()))[0]->asObject())[name]->asString()->val();
        }
        else{
            set_symbol = (*(set_field->asObject()))[name]->asString()->val();
        }

        //new global variable is allocated and its components are set
        DDebug::global_var* global_var_temp = new DDebug::global_var;
        if(hasDefault)
            global_var_temp->get = reinterpret_cast<bool (*)(Diderot_world_t*, void*)>(dlsym(handle,get_symbol.c_str()));
        global_var_temp->set = reinterpret_cast<bool (*)()>(dlsym(handle,set_symbol.c_str()));
        global_var_temp->tydesc = tyd;
        if(tyd != nullptr)
            global_var_temp->val = tyd->new_value();
        global_var_temp->definedDefault = hasDefault;
        (*glob).insert(std::pair<string, DDebug::global_var*>(key, global_var_temp));
    }   
    return 0;
}

//follows the same logic as global, but no set function
bool DDebug::load_local_functions(void* handle, JSON::value* json_value, DDebug::locals_function* loc){
    /* Open JSON file here
     * Loop through all the vars of strand and load the information
     * into an array of locals_function structs
     * Note: Will use dlsym*/
    DDebug::tydesc_factory *tyfact = nullptr;
    if(!json_value){
        cout << "JSON value uninitialized.\n";
        return 1;
    }
    string strand = "strand";
    string get = "get";
    string state_vars = "state-vars";
    string name = "name";
    string type = "type";
    const JSON::object* jobj = json_value->asObject();
    JSON::value* jval = (*((*jobj)[strand])->asObject())[state_vars];
    const JSON::array* strand_arry = jval->asArray();
    bool isLInt = isLongInt(json_value);
    bool isDbl = isDouble(json_value);
    int length = strand_arry->length();
    int i;
    for(i = 0; i < length; i++){
        if(tyfact)
            delete tyfact;
        tyfact = new DDebug::tydesc_factory(isLInt, isDbl);
        const DDebug::type_desc* tyd = tyfact->mk_tydesc(*((*(*strand_arry)[i]->asObject())[type]));

        string key = (((*(*strand_arry)[i]->asObject())[name])->asString())->val();
        string symbol = (*(((*(*strand_arry)[i]->asObject())[get])->asObject()))[name]->asString()->val();

        DDebug::local_var* local_var_temp = new DDebug::local_var;
        local_var_temp->get = reinterpret_cast<void* (*)(Diderot_world_t*, uint32_t)>(dlsym(handle,symbol.c_str()));
        local_var_temp->tydesc = tyd;
        if(tyd != nullptr)
            local_var_temp->val = tyd->new_value();
        (*loc).insert(std::pair<string, DDebug::local_var*>(key, local_var_temp));

    }   
    return 0;
}

//FIXME: Temporary symbols in the style of "Diderot_...."
//Will implement dynamic loading of runtime functions in the future. 
//I've done some of it in the commented out section
bool DDebug::load_runtime_functions(void* handle, JSON::value* json_value, DDebug::runtime_functions* rf){
    
    /*string runtime = "runtime";
    string func = "func";
    string name = "name";
    const JSON::object* jobj = json_value->asObject();
    JSON::value* jval = (*((*jobj)[runtime])->asObject())[runtime];
    const JSON::array* runtime_arry = jval->asArray();
    if(!(new_world = 
        reinterpret_cast<Diderot_world_t* (*)()>(dlsym(handle, 
                (*(*(*runtime_arry)[0]->asObject())[func]->asObject())[name]->asString()->val().c_str()))))
        cout << "WARNING: Diderot_init_world not found\n";
    if(!(init_world = 
        reinterpret_cast<bool (*)(Diderot_world_t*)>
            (dlsym(handle, 
                (*(*(*runtime_arry)[1]->asObject())[func]->asObject())[name]->asString()->val().c_str()))))
        cout << "WARNING: Diderot_new_world not found\n";
    if(!(create_strands = 
        reinterpret_cast<bool (*)(Diderot_world_t*)>
            (dlsym(handle, 
                (*(*(*runtime_arry)[2]->asObject())[func]->asObject())[name]->asString()->val().c_str()))))
        cout << "WARNING: Diderot_create_strands not found \n";
    if(!(run = 
        reinterpret_cast<uint32_t 
            (*)(Diderot_world_t*, uint32_t)>(dlsym(handle,
                (*(*(*runtime_arry)[3]->asObject())[func]->asObject())[name]->asString()->val().c_str()))))
        cout << "WARNING: Diderot_run not found\n";
    if(!(set_verbose = 
        reinterpret_cast<void 
            (*)(Diderot_world_t*, bool)>(dlsym(handle, "Diderot_set_verbose"))))
        cout << "WARNING: Diderot_set_verbose not found \n";*/
    if(!(rf->init_world = 
        reinterpret_cast<bool 
            (*)(Diderot_world_t*)>(dlsym(handle, "Diderot_init_world"))))
        cout << "WARNING: Diderot_init_world not found\n";
    if(!(rf->new_world = 
        reinterpret_cast<Diderot_world_t* 
            (*)()>(dlsym(handle, "Diderot_new_world"))))
        cout << "WARNING: Diderot_new_world not found\n";
    if(!(rf->create_strands = 
        reinterpret_cast<bool 
            (*)(Diderot_world_t*)>(dlsym(handle, "Diderot_create_strands"))))
        cout << "WARNING: Diderot_create_strands not found \n";
    if(!(rf->run = 
        reinterpret_cast<uint32_t 
            (*)(Diderot_world_t*, uint32_t)>(dlsym(handle,"Diderot_run"))))
        cout << "WARNING: Diderot_run not found\n";
    if(!(rf->set_verbose = 
        reinterpret_cast<void 
            (*)(Diderot_world_t*, bool)>(dlsym(handle, "Diderot_set_verbose"))))
            cout << "WARNING: Diderot_set_verbose not found \n";
    if(!(rf->num_strands = reinterpret_cast<uint32_t
        (*)(Diderot_world_t*)>(dlsym(handle, "Diderot_num_strands"))))
        cout << "WARNING: Diderot_num_strands not found \n";
    if(!(rf->num_active_strands = reinterpret_cast<uint32_t 
        (*)(Diderot_world_t*)>(dlsym(handle, "Diderot_num_active_strands"))))
        cout << "WARNING: Diderot_num_active_strands not found \n";
    if(!(rf->num_stable_strands = reinterpret_cast<uint32_t 
        (*)(Diderot_world_t*)>(dlsym(handle, "Diderot_num_stable_strands"))))
        cout << "WARNING: Diderot_num_stable_strands not found \n";
    if(!(rf->shutdown = reinterpret_cast<void 
        (*)(Diderot_world_t*)>(dlsym(handle, "Diderot_shutdown"))))
        cout << "WARNING: Diderot_shutdown not found \n";
    return 1;
}

/*DDebug::global_var::~global_var(){
    if(val)
        delete val;
    if(tydesc)
        delete tydesc;
}*/