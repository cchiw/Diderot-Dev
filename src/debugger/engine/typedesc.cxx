/*! \file typedesc.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#include <stdexcept>
#include <iostream>
#include <sstream> 
#include "typedesc.hxx"
#include "value.hxx"

namespace DDebug {

  /********** class type_desc member functions **********/

    type_desc::~type_desc () { }

  /********** class bool_tydesc member functions **********/

    size_t bool_tydesc::size_in_bytes () const
    {
        return sizeof(bool);
    }

    size_t bool_tydesc::alignment () const
    {
        return sizeof(bool);
    }

    std::string const bool_tydesc::to_string () const
    {
        return "bool";
    }

    value *bool_tydesc::new_value () const
    {
        return new bool_value (this);
    }

    value *bool_tydesc::from_string (std::string const &str) const
    {
        bool val;        
        if (str.compare("true") == 0) {
            val = true;
	}
        else if (str.compare("false") == 0) {
            val = false;
	}
        else {
            return nullptr; //error if not true or false
        }        
        value* bool_val = this->new_value();
        bool_val->init(&val);
        return bool_val;
    }

  /********** class int_tydesc member functions **********/

    std::string const int_tydesc::to_string () const
    {
        return "int";
    }

  /********** class int32_tydesc member functions **********/

    size_t int32_tydesc::size_in_bytes () const
    {
        return sizeof(int32_t);
    }

    size_t int32_tydesc::alignment () const
    {
        return 4;
    }

    value *int32_tydesc::new_value () const
    {
        return new int32_value (this);
    }
    
    value *int32_tydesc::from_string (std::string const &str) const
    {
        try {
            int32_t val = std::stoi(str);
	    value *int_val = this->new_value();
	    int_val->init (&val);
	    return int_val;
        } catch(std::invalid_argument& e) {
	  // invalid input for an int
            return nullptr;
        } catch(std::out_of_range& e) {
	  // integer given is out of bounds for int32
            return nullptr;
        }
    }

  /********** class int64_tydesc member functions **********/

    size_t int64_tydesc::size_in_bytes () const
    {
        return sizeof(int64_t);
    }

    size_t int64_tydesc::alignment () const
    {
        return 8;
    }

    value *int64_tydesc::new_value () const
    {
        return new int64_value (this);
    }
    
    value *int64_tydesc::from_string (std::string const &str) const
    {
        try {
            int64_t val = std::stoll(str);
	    value* int_val = this->new_value();
	    int_val->init (&val);
	    return int_val;
        } catch(std::invalid_argument& e) {
	  //invalid input for an int
            return nullptr;
        } catch(std::out_of_range& e) {
	  //integer given is out of bounds for int_64
            return nullptr;
        }
    }

  /********** class string_tydesc member functions **********/

    size_t string_tydesc::size_in_bytes () const
    {
        return 0;  // dynamic size
    }

    size_t string_tydesc::alignment () const
    {
        return 1;
    }

    std::string const string_tydesc::to_string () const
    {
        return "string";
    }

    value *string_tydesc::new_value () const
    {
        return new string_value (this);
    }
    
    value *string_tydesc::from_string (std::string const &str) const
    {
	//value* str_val = this->new_value();
        //str_val->valueof() = str;
        return nullptr;
    }

  /********** class tensor_tydesc member functions **********/

    tensor_tydesc::~tensor_tydesc () { }

    tensor_tydesc::tensor_tydesc (std::vector<uint32_t> const &shp)
      : _shp(shp), _size(1)
    {
        for (auto it = shp.cbegin();  it != shp.cend();  ++it) {
            this->_size *= *it;
        }
    }

    std::string const tensor_tydesc::to_string () const
    {
        if (this->_size == 1) {
            return "real";
        }
        else if (this->_shp.size() == 1) {
            switch (this->_shp[0]) {
                case 2: return "vec2";
                case 3: return "vec3";
                case 4: return "vec4";
            }
        }

        auto it = this->_shp.cbegin();
        std::string name = "tensor[" + std::to_string(*it);
        ++it;
        for (;  it != this->_shp.cend();  ++it) {
            name += "," + std::to_string(*it);
        }
        name += "]";
        return name;
    }
    
    

  /********** class tensor32_tydesc member functions **********/

    size_t tensor32_tydesc::size_in_bytes () const
    {
        return sizeof(float) * this->_size;
    }

    size_t tensor32_tydesc::alignment () const
    {
        return 4;
    }

    value *tensor32_tydesc::new_value () const
    {
        return new tensor32_value (this);
    }
    
    value *tensor32_tydesc::from_string (std::string const &str) const
    {
        value *tensor_val = this->new_value();
        if (this->_size == 1) {
            float val;
            try {
                val = std::stof(str);
            } catch(std::invalid_argument& e) {
	      // invalid input for a float
                return nullptr;
            } catch(std::out_of_range& e) {
	      // integer given is out of bounds for float
                return nullptr;
            }
            tensor_val->init(&val);
        }
	//first order tensor
        else if (this->_shp.size() == 1) {
	    /* Because getline can only use one delimiter at a time to
	     * tokenize, three loops will be needed for '[' and ',' and ']'. */
	    // first stream is initialized and will be tokenized with the ',' delimiter
	    std::istringstream comma_tok(str);
            std::string temp_0, temp_1, temp_2;
	    //temporary array that will be used to initialized the value
            float val[this->_shp[0]];
            unsigned int i = 0;
	    // the first loop tokenizes "str" with the ',' delimiter
            while (std::getline(comma_tok, temp_0, ',')) {
		/* "std::getline" stores whatever the tokenizer finds 
		 * before/between commas into temp_0.
		 * Then, a second stream is initialized with temp_0 and that 
		 * will be further tokenized with '[' into temp_1 */                 
		std::istringstream o_b_tok(temp_0);
                while (std::getline(o_b_tok, temp_1, '[')) {
		    // as with above, a third stream is initialized with temp_1
                    std::istringstream c_b_tok(temp_1);
                    while (std::getline(c_b_tok, temp_2, ']')) {
			/* This check is necessary because the string before '[' 
			 * in a tensor representation such as [1,2] is "" */
                        if (temp_1.compare("")) {
                            try {
				/* The value of the i-th index is finally parsed
				 * and now put into the correct index of the temporary array*/ 
                                val[i] = std::stof(temp_1);
                                i++;
                            } catch(std::invalid_argument& e) {
                                //invalid input for a float
                                return nullptr;
                            } catch(std::out_of_range& e) {
                                //integer given is out of bounds for float
                                return nullptr;
                            } 
                        }
                    }
                }   
            }
            tensor_val->init(val);
        }
	else {
	    /* FIXME: higher-order tensor */
	    return nullptr;
	}
	return tensor_val;
    }

  /********** class tensor64_tydesc member functions **********/

    size_t tensor64_tydesc::size_in_bytes () const
    {
        return sizeof(double) * this->_size;
    }

    size_t tensor64_tydesc::alignment () const
    {
        return 8;
    }

    value *tensor64_tydesc::new_value () const
    {
        return new tensor64_value (this);
    }

    value *tensor64_tydesc::from_string (std::string const &str) const
    {
	/* See tensor32_tydesc::from_string above for near identical comments.
	 * The only differences are double vs float and stod vs stof */
        value* tensor_val = this->new_value();
        if(this->_size == 1)
        {
            double val;
            try {
                val = std::stod(str);
            } catch(std::invalid_argument& e) {
                //invalid input for a float
                return nullptr;
            } catch(std::out_of_range& e) {
                //integer given is out of bounds for float
                return nullptr;
            }
            tensor_val->init(&val);
        }
        else if (this->_shp.size() == 1)
        {
            std::istringstream comma_tok(str);
            std::string temp_0, temp_1, temp_2;
            double val[this->_shp[0]];
            unsigned int i = 0;
            while (std::getline(comma_tok, temp_0, ',')) {
                std::istringstream o_b_tok(temp_0);
                while (std::getline(o_b_tok, temp_1, '[')) {
                    std::istringstream c_b_tok(temp_1);
                    while (std::getline(c_b_tok, temp_2, ']')) { 
                        if (temp_1.compare("")) {
                            try {
                                val[i] = std::stod(temp_1);
                                i++;
                            } catch(std::invalid_argument& e) {
                                //invalid input for a float
                                return nullptr;
                            } catch(std::out_of_range& e) {
                                //integer given is out of bounds for double
                                return nullptr;
                            } 
                        }
                    }
                }   
            }
            tensor_val->init(val);
        }
	else {
	    /* FIXME: higher-order tensor */
	    return nullptr;
	}
	return tensor_val;
    }

  /********** class seq_tydesc member functions **********/

    seq_tydesc::~seq_tydesc ()
    {
        // NOTE: we do not free this->_elemTy to avoid multiple frees?  Deleting the
        // tydesc_factory should reclaim all of the type descriptors
    }

    size_t seq_tydesc::size_in_bytes () const
    {
        return this->elem_typedesc()->size_in_bytes() * this->_size;
    }

    size_t seq_tydesc::alignment () const
    {
        return this->elem_typedesc()->alignment();
    }

    std::string const seq_tydesc::to_string () const
    {
        return this->elem_typedesc()->to_string() + "[" + std::to_string(this->_size) + "]";
    }

    value *seq_tydesc::new_value () const
    {
        return new seq_value (this);
    }
    
    value *seq_tydesc::from_string (std::string const &str) const
    {
	return nullptr;
    }

  /********** class dynseq_tydesc member functions **********/

    dynseq_tydesc::~dynseq_tydesc ()
    {
        // NOTE: we do not free this->_elemTy to avoid multiple frees?  Deleting the
        // tydesc_factory should reclaim all of the type descriptors
    }

    size_t dynseq_tydesc::size_in_bytes () const
    {
        return 0;  // dynamic size
    }

    size_t dynseq_tydesc::alignment () const
    {
        return this->elem_typedesc()->alignment();
    }

    std::string const dynseq_tydesc::to_string () const
    {
        return this->elem_typedesc()->to_string() + "[]";
    }

    value *dynseq_tydesc::new_value () const
    {
        return new dynseq_value (this);
    }
    
    value *dynseq_tydesc::from_string (std::string const &str) const
    {
	return nullptr;
    }

  /********** class image_tydesc member functions **********/

    image_tydesc::~image_tydesc () { }

    size_t image_tydesc::size_in_bytes () const // will return 0
    {
        return 0;  // dynamic size
    }

    size_t image_tydesc::alignment () const
    {
        return sizeof(void *); // FIXME: not clear what this value should be
    }

    std::string const image_tydesc::to_string () const
    {
        std::string name = "image(" + std::to_string(this->_dim) + ")[";
        if (this->_shp.size() > 0) {
            auto it = this->_shp.cbegin();
            name += std::to_string(*it);
            ++it;
            for (;  it != this->_shp.cend();  ++it) {
                name += "," + std::to_string(*it);
            }
        }
        name += "]";
        return name;
    }

    value *image_tydesc::new_value () const
    {
        return new image_value (this);
    }
    
    value *image_tydesc::from_string (std::string const &str) const
    {
	value* img_val = this->new_value();
        //img_val->valueof() = str;
        return nullptr;
    }

} // namespace DDebug
