/*! \file typedesc.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _TYPEDESC_HXX_
#define _TYPEDESC_HXX_

namespace DDebug {

  // the base class of descriptors for representing Diderot types
    class TypeDesc {
      public:

	virtual size_t sizeB () const = 0;

      protected:
    };

  // for the Diderot type 'bool'
    class BoolTypeDesc : public TypeDesc {
      public:
    };

  // for the Diderot type 'int'
    class IntTypeDesc : public TypeDesc {
      public:
    };

    class StringTypeDesc : public TypeDesc {
      public:
    };

  // for the Diderot types 'tensor[shape]'
    class TensorTypeDesc : public TypeDesc {
      public:
	int order () const;
	const int *shape () const;
    };

    class SeqTypeDesc : public TypeDesc {
      public:
    };

    class DynseqTypeDesc : public TypeDesc {
      public:
    };

}

#endif // !_TYPEDESC_HXX_
