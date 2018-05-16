#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <dlfcn.h>
#include <string>
#include <stdint.h>
#include <fstream>
#include <sstream>
#include <map>
#include "tydesc-factory.hxx"
#include "json.hxx"
#include "value.hxx"
#include "typedesc.hxx"

#ifndef DEBUG_H
#define DEBUG_H

//FIXME: change the structure of the classes to make val and tydesc private.

struct Diderot_world_t;
using namespace std;
namespace DDebug{

    class variable{
    public:   
        variable(){};
        bool (*set)() = nullptr;
        template <typename setval> bool set_value(Diderot_world_t* wrld, setval val){
            bool (*set_func)(Diderot_world_t*, setval); 
            if(set){
                set_func = reinterpret_cast<bool (*)(Diderot_world_t*, setval)>(set);
                return set_func(wrld, val);
            }
            else{
                cout << "Set function uninitialized\n";
                return 1;
            }
        }
        //TODO: when dynseq, img, and strings implemented:
        //uint32_t (*get_size)(Diderot_world_t*); 
        value* val;
        const type_desc* tydesc;
    };

    class local_var : public variable {
        public:
            void* (*get)(Diderot_world_t*, uint32_t);
            local_var(){};
    };

    class global_var : public variable {
        public:
            bool (*get)(Diderot_world_t*, void*);  
            global_var(){};
            ~global_var();
            bool definedDefault;
    }; 

    typedef struct runtime_functions{
        Diderot_world_t* (*new_world)();
        bool (*init_world)(Diderot_world_t*);
        bool (*create_strands)(Diderot_world_t*);
        uint32_t (*run)(Diderot_world_t*, uint32_t);
        void (*set_verbose)(Diderot_world_t*, bool);
        uint32_t (*num_strands)(Diderot_world_t*);
        uint32_t (*num_stable_strands)(Diderot_world_t*);
        uint32_t (*num_active_strands)(Diderot_world_t*);
        void (*shutdown)(Diderot_world_t*);
    }runtime_functions;

typedef map<string, local_var*> locals_function;

typedef map<string, global_var*> globals_function;

bool load_local_functions(void* handle, JSON::value* json_value, DDebug::locals_function* loc);

bool load_global_functions(void* handle, JSON::value* json_value, DDebug::globals_function* glob);

bool load_runtime_functions(void* handle, JSON::value* json_value, DDebug::runtime_functions* rf);


}

#endif