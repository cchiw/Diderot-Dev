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

#include <string>
#include <cstdint>
#include <vector>

namespace DDebug {

    class value;

  //! the base class of descriptors for representing Diderot types
    class type_desc {
      public:

        virtual ~type_desc ();

      //! size of values in bytes; returns 0 for dynamically sized values
        virtual size_t size_in_bytes () const = 0;

      //! alignment of values in bytes
        virtual size_t alignment () const = 0;

      //! return string representation of the type
        virtual std::string const to_string () const = 0;

      //! create an uninitialized value of this type
        virtual value *new_value () const = 0;

      //! create a value intialized from a string
	virtual value *from_string (std::string const &str) const = 0;

      //! do values of this type have static size?
        bool hasStaticSize () const { return this->size_in_bytes() > 0; }

    };

  // for the Diderot type 'bool'
    class bool_tydesc : public type_desc {
      public:

        size_t size_in_bytes () const;
        size_t alignment () const;
        std::string const to_string () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

    };

  // for the Diderot type 'int'
    class int_tydesc : public type_desc {
      public:

        virtual size_t size_in_bytes () const = 0;
        virtual size_t alignment () const = 0;
        std::string const to_string () const;
        virtual value *new_value () const = 0;
	virtual value *from_string (std::string const &str) const = 0;

    };

  // for the Diderot type 'int'
    class int32_tydesc : public int_tydesc {
      public:

        size_t size_in_bytes () const;
        size_t alignment () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

    };

  // for the Diderot type 'int' (--longint option)
    class int64_tydesc : public int_tydesc {
      public:

        size_t size_in_bytes () const;
        size_t alignment () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

    };

    class string_tydesc : public type_desc {
      public:

        size_t size_in_bytes () const;
        size_t alignment () const;
        std::string const to_string () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

    };

  // for the Diderot types 'tensor[shape]'
    class tensor_tydesc : public type_desc {
      public:

        virtual ~tensor_tydesc ();

        uint32_t size () const { return this->_size; }
        virtual size_t size_in_bytes () const = 0;
        virtual size_t alignment () const = 0;
        std::string const to_string () const;
        virtual value *new_value () const = 0;
	virtual value *from_string (std::string const &str) const = 0;

        uint32_t order () const { return this->_shp.size(); }
        std::vector<uint32_t> const &shape () const { return this->_shp; }

      protected:
        std::vector<uint32_t>   _shp;           //!< shape of tensor
        uint32_t                _size;          //!< number of elements

        explicit tensor_tydesc (std::vector<uint32_t> const &shp);

    };

  // for the Diderot types 'tensor[shape]'
    class tensor32_tydesc : public tensor_tydesc {
      public:

        explicit tensor32_tydesc (std::vector<uint32_t> const &shp) : tensor_tydesc(shp) { }
        ~tensor32_tydesc () { }

        size_t size_in_bytes () const;
        size_t alignment () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

    };

  // for the Diderot types 'tensor[shape]' (--double option)
    class tensor64_tydesc : public tensor_tydesc {
      public:

        explicit tensor64_tydesc (std::vector<uint32_t> const &shp) : tensor_tydesc(shp) { }
        ~tensor64_tydesc () { }

        size_t size_in_bytes () const;
        size_t alignment () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

    };

    class seq_tydesc : public type_desc {
      public:
        seq_tydesc (const type_desc *td, int n) : type_desc(), _elemTy(td), _size(n) { }
        ~seq_tydesc ();

        size_t size_in_bytes () const;
        size_t alignment () const;
        std::string const to_string () const;

      // return size of fixed-size sequence type
        int size () const { return this->_size; }
        const type_desc *elem_typedesc () const { return this->_elemTy; }
        value *new_value () const;
	value *from_string (std::string const &str) const;

      protected:
        const type_desc *_elemTy;
        int _size;
    };

    class dynseq_tydesc : public type_desc {
      public:
        dynseq_tydesc (const type_desc *td) : type_desc(), _elemTy(td) { }

        ~dynseq_tydesc ();

        size_t size_in_bytes () const; // will return 0
        size_t alignment () const;
        std::string const to_string () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

        const type_desc *elem_typedesc () const { return this->_elemTy; }

      protected:
        const type_desc *_elemTy;
    };

    class image_tydesc : public type_desc {
      public:

        image_tydesc (uint32_t dim, std::vector<uint32_t> const &shp)
          : _shp(shp), _dim(dim)
        { }

        ~image_tydesc ();

        size_t size_in_bytes () const; // will return 0
        size_t alignment () const;
        std::string const to_string () const;
        value *new_value () const;
	value *from_string (std::string const &str) const;

        uint32_t dimension () const { return this->_dim; }
        uint32_t order () const { return this->_shp.size(); }
        std::vector<uint32_t> const &shape () const { return this->_shp; }

      private:
        std::vector<uint32_t>   _shp;           //!< shape of voxel
        uint32_t                _dim;           //!< dimension of image

    };

}

#endif // !_TYPEDESC_HXX_
