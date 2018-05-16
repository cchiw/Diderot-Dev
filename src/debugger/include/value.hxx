/*! \file value.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _VALUE_HXX_
#define _VALUE_HXX_

#include "typedesc.hxx"
#include <string>
#include <cstring>
#include <cstdlib>
#include <iostream>

namespace DDebug {

    class value {
      public:

        virtual ~value();

        const type_desc *typedesc () const { return this->_tydesc; }

        virtual void *addrof () = 0;
        virtual size_t size_in_bytes () const;
	virtual std::string to_string() const {return std::to_string(0); }

      // initialize a value
        void init (const void *src)
        {
// FIXME: this method may not work for dynamically sized values!!!
            std::memcpy (this->addrof(), src, this->size_in_bytes());
        }

      protected:
        const type_desc *_tydesc;

        explicit value (const type_desc *td) : _tydesc(td) { }
    };

  // for the Diderot type 'bool'
    class bool_value : public value {
      public:

        bool_value (const type_desc *td) : value(td) { }
        ~bool_value();

        bool valueof () const { return this->_value; }

        void *addrof ();
	
	std::string to_string() const {return std::to_string(this->_value);};

      private:
        bool _value;

    };

  // for the Diderot type 'int'
    class int32_value : public value {
      public:

        int32_value (const type_desc *td) : value(td) { }
        ~int32_value();

        int32_t valueof () const { return this->_value; }

        void *addrof ();
	
	std::string to_string() const {return std::to_string(this->_value);};

      private:
        int32_t _value;

    };

  // for the Diderot type 'int' (with --long-int option)
    class int64_value : public value {
      public:

        int64_value (const type_desc *td) : value(td) { }
        ~int64_value();

        int64_t valueof () const { return this->_value; }

        void *addrof ();
	
	std::string to_string() const {return std::to_string(this->_value);}

      private:
        int64_t _value;
    };

  // for the Diderot type 'string'
    class string_value : public value {
      public:

        string_value (const type_desc *td) : value(td) { }
        ~string_value();

        std::string const &valueof () const { return this->_value; }

        void *addrof ();
        size_t size_in_bytes () const;

	std::string to_string() const {return this->_value;}

      private:
        std::string _value;
    };

  // for the Diderot types 'tensor[shape]'
    class tensor32_value : public value {
      public:

        tensor32_value (const type_desc *td) : value(td)
        {
            const tensor32_tydesc *tenTD = dynamic_cast<const tensor32_tydesc *>(td);
            if (tenTD != nullptr) {
                this->_value.reserve(tenTD->size());
            }
// FIXME: internal error
        }
        ~tensor32_value();

        int order () const;
        const int *shape () const;

        std::vector<float> valueof () { return this->_value; }
        std::vector<float> const &valueof () const { return this->_value; }
	
	std::string to_string() const {
		std::string str = "[";
		int size;
		if((size = this->_value.capacity()) > 1){
			for(int i = 0; i < size; i++){			
				str = str + std::to_string(this->_value[i]) + ", "; 			
			}			
			return str + "]";
		}
		else{
			return std::to_string(this->_value[0]);
		}	
	}

        void *addrof ();
	
      private:
        std::vector<float> _value;
    };

  // for the Diderot types 'tensor[shape]' (with '--double' option)
    class tensor64_value : public value {
      public:

        tensor64_value (const type_desc *td) : value(td)
        {
            const tensor64_tydesc *tenTD = dynamic_cast<const tensor64_tydesc *>(td);
            if (tenTD != nullptr) {
                this->_value.reserve(tenTD->size());
            }
// FIXME: internal error
        }
        ~tensor64_value();

        int order () const;
        const int *shape () const;
	
	std::vector<double> valueof () { return this->_value; }
        std::vector<double> const &valueof () const { return this->_value; }

        void *addrof ();
	std::string to_string() const {return std::to_string(0);}

      private:
        std::vector<double> _value;
    };

    class seq_value : public value {
      public:

        seq_value (const type_desc *td) : value(td)
        {
            const seq_tydesc *seqTD = dynamic_cast<const seq_tydesc *>(td);
            if (seqTD != nullptr) {
                this->_values = std::malloc(seqTD->size() * seqTD->elem_typedesc()->size_in_bytes());
            }
            else {
// FIXME: internal error
                this->_values = nullptr;
            }
        }
        ~seq_value();

        int length () const
        {
            const seq_tydesc *seqTD = dynamic_cast<const seq_tydesc *>(this->_tydesc);
            if (seqTD != nullptr) {
                return seqTD->size();
            }
            else {
// FIXME: internal error
                return -1;
            }
        }

        const type_desc *elem_typedesc () const
        {
            const seq_tydesc *seqTD = dynamic_cast<const seq_tydesc *>(this->_tydesc);
            if (seqTD != nullptr) {
                return seqTD->elem_typedesc();
            }
            else {
// FIXME: internal error
                return nullptr;
            }
        }

        void *addrof ();
        size_t size_in_bytes () const;
	std::string to_string() const {return std::to_string(0);}

      private:
        void *_values;
    };

  // for the Diderot dynamic sequence types
    class dynseq_value : public value {
      public:

        dynseq_value (const type_desc *td) : value(td) { }
        ~dynseq_value();

        int length () const { return this->_values.size(); }

        const type_desc *elem_typedesc () const
        {
            return dynamic_cast<const dynseq_tydesc *>(this->_tydesc)->elem_typedesc();
        }

        void *addrof ();
        size_t size_in_bytes () const;
	std::string to_string() const {return std::to_string(0);}

      private:
        std::vector<value> _values;
    };

  // for the Diderot image types.  For now, the debugger-side representation is the file
  // name of the image-nrrd file.
    class image_value : public value {
      public:
        image_value (const type_desc *td) : value(td) { }
        ~image_value ();

        uint32_t dim () const;
        uint32_t order () const;
        const int *shape () const;
	
        void *addrof ();
	std::string to_string() const {return this->_imgFile;}
      private:
        std::string _imgFile;

    };

} // DDebug

#endif // !_VALUE_HXX_
