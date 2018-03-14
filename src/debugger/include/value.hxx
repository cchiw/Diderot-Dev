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

namespace DDebug {

    class Value {
      public:

	virtual TypeDesc *typeOf () const = 0;
	virtual void *addrOf () const = 0;

	virtual ~Value();

      protected:
	explicit Value () { }
    };

  // for the Diderot type 'bool'
    class BoolValue : public Value {
      public:
	bool value () const { return this->_value; }

	TypeDesc *typeOf () const;
	void *addrOf () const;

	~BoolValue() { }

    };

  // for the Diderot type 'int'
    class IntValue : public Value {
      public:
	int32_t value () const { return this->_value; }

	TypeDesc *typeOf () const;
	void *addrOf () const;

      private:
	int32_t _value;

    };

  // for the Diderot type 'int' (for --long-int option)
    class LongIntValue : public Value {
      public:
	int64_t value () const { return this->_value; }

	TypeDesc *typeOf () const;
	void *addrOf () const;

      private:
	int64_t _value;
    };

    class StringValue : public Value {
      public:
    };

  // for the Diderot types 'tensor[shape]'
/* FIXME: need both float and double versions! */
    class TensorValue : public Value {
      public:
	int order () const;
	const int *shape () const;

	TypeDesc *typeOf () const;
	void *addrOf () const;
    };

    class SeqValue : public Value {
      public:

	TypeDesc *typeOf () const;
	void *addrOf () const;
    };

  // for the Diderot dynamic sequence types
    class DynseqValue : public Value {
      public:

	TypeDesc *typeOf () const;
	void *addrOf () const;
    };

} // DDebug

#endif // !_VALUE_HXX_
