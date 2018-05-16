/*! \file value.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#include "typedesc.hxx"
#include "value.hxx"

namespace DDebug {

  /********** class Value member functions **********/

    value::~value () { }

    size_t value::size_in_bytes () const
    {
      // default implementation gets the size from the value's type
        return this->_tydesc->size_in_bytes();
    }

  /********** class bool_value member functions **********/

    bool_value::~bool_value () { }

    void *bool_value::addrof () { return &this->_value; }

  /********** class int32_value member functions **********/

    int32_value::~int32_value () { }

    void *int32_value::addrof () { return &this->_value; }

  /********** class int64_value member functions **********/

    int64_value::~int64_value () { }

    void *int64_value::addrof () { return &this->_value; }

  /********** class string_value member functions **********/

    string_value::~string_value () { }

    void *string_value::addrof () { return nullptr; /* FIXME */ }

    size_t string_value::size_in_bytes () const
    {
        return this->_value.size() + 1;
    }


  /********** class tensor32_value member functions **********/

    tensor32_value::~tensor32_value () { }

    void *tensor32_value::addrof () { return this->_value.data(); }

  /********** class tensor64_value member functions **********/

    tensor64_value::~tensor64_value () { }

    void *tensor64_value::addrof () { return this->_value.data(); }

  /********** class seq_value member functions **********/

    seq_value::~seq_value ()
    {
        if (this->_values != nullptr) {
            std::free (this->_values);
        }
    }

    void *seq_value::addrof () { return nullptr; /* FIXME */ }

    size_t seq_value::size_in_bytes () const
    {
      // default implementation gets the size from the value's type
        return this->elem_typedesc()->size_in_bytes() * this->length();
    }

  /********** class dynseq_value member functions **********/

    dynseq_value::~dynseq_value () { }

    void *dynseq_value::addrof () { return nullptr; /* FIXME */ }

    size_t dynseq_value::size_in_bytes () const
    {
      // default implementation gets the size from the value's type
        return this->elem_typedesc()->size_in_bytes() * this->length();
    }

  /********** class image_value member functions **********/

    image_value::~image_value () { }

    void *image_value::addrof () { return nullptr; /* FIXME */ }

} // namespace DDebug
