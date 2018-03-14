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

#include "typedesc.hxx"
#include "value.hxx"

namespace DDebug {

  /********** class BoolTypeDesc member functions **********/

    size_t BoolTypeDesc::size_in_bytes ()
    {
	return sizeof(bool);
    }

    Value *BoolTypeDesc::alloc ()
    {
	return new BoolValue();
    }

    std::string BoolTypeDesc::to_string ()
    {
	return "bool";
    }

  /********** class IntTypeDesc member functions **********/

    size_t IntTypeDesc::size_in_bytes ()
    {
	return sizeof(int32_t);
    }

    Value *IntTypeDesc::alloc ()
    {
	return new IntValue();
    }

    std::string IntTypeDesc::to_string ()
    {
	return "int";
    }

  /********** class LongIntTypeDesc member functions **********/

    size_t LongIntTypeDesc::size_in_bytes ()
    {
	return sizeof(int);
    }

    Value *LongIntTypeDesc::alloc ()
    {
	return new LongIntValue();
    }

    std::string LongIntTypeDesc::to_string ()
    {
	return "int";
    }

  /********** class StringTypeDesc member functions **********/

  /********** class TensorTypeDesc member functions **********/
/* FIXME: need both float and double versions! */

  /********** class SeqTypeDesc member functions **********/

  /********** class DynseqTypeDesc member functions **********/

  /**** canonical type descriptors *****/
    BoolTypeDesc *BoolType = nullptr;
    IntTypeDesc *IntType= nullptr;
    IntTypeDesc *LongIntType= nullptr;
    StringTypeDesc *StringType= nullptr;

    void init_type_descs ()
    {
	BoolType = new BoolTypeDesc;
	IntType = new IntTypeDesc;
	LongIntType = new LongIntTypeDesc;
	StringType = new StringTypeDesc;
    }

} // namespace DDebug
