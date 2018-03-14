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

    Value::~Value () { }

  /********** class BoolValue member functions **********/

    TypeDesc TypeDesc *BoolValue::typeOf () const
    {
	return BoolType;
    }

    void *BoolValue::addrOf () const
    {
	return &this->_value;
    }

  /********** class IntValue member functions **********/

    TypeDesc TypeDesc *IntValue::typeOf () const
    {
	return IntType;
    }

    void *IntValue::addrOf () const
    {
	return &this->_value;
    }

  /********** class LongIntValue member functions **********/

    TypeDesc TypeDesc *LongIntValue::typeOf () const
    {
	return LongIntType;
    }

    void *LongIntValue::addrOf () const
    {
	return &this->_value;
    }

  /********** class TensorValue member functions **********/

  /********** class SeqValue member functions **********/

  /********** class DynseqValue member functions **********/

} // namespace DDebug
