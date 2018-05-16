/*! \file tydesc-factory.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _TYDESC_FACTORY_HXX_
#define _TYDESC_FACTORY_HXX_

#include "json.hxx"
#include "typedesc.hxx"

namespace DDebug {

    class tydesc_factory {
      public:

        tydesc_factory (bool longInt, bool doubleReal);
        ~tydesc_factory ();

        const type_desc *mk_bool_tydesc ();
        const type_desc *mk_int_tydesc ();
        const type_desc *mk_string_tydesc ();

        const type_desc *mk_tydesc (JSON::value const &desc);

      private:
        bool _longInt;                  // true, if Diderot int is 64 bits (--longint option)
        bool _doubleReal;               // true, if Diderot real is 64 bits
        type_desc *_bool;               // type descriptor for Diderot "bool" type
        type_desc *_int;                // type descriptor for Diderot "int" type
        type_desc *_string;             // type descriptor for Diderot "string" type

        struct hash_table *_tbl;

        const type_desc *from_json (JSON::value const *desc);
    };

} // namespace DDebug

#endif // !_TYDESC_FACTORY_HXX_
