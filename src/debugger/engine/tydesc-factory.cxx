/*! \file tydesc-factory.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#include "tydesc-factory.hxx"
#include <functional>
#include <unordered_map>

// instantiate std::hash for JSON values
namespace std{
    template<>
    struct hash<JSON::value> {
        size_t operator()(JSON::value const &v) { return v.hash(); }
    };
}

namespace DDebug {

    struct hash_json {
        size_t operator()(JSON::value const *v) const { return v->hash(); }
    };

    struct eql_json {
        bool operator()(JSON::value const *lhs, JSON::value const *rhs) const
        {
            return true;
        }
    };

    typedef std::unordered_map<const JSON::value *, const type_desc *, hash_json, eql_json> json_map;

    struct hash_table {
        json_map _tbl;

        hash_table () : _tbl(100) { }

        void insert (std::initializer_list<json_map::value_type> il)
        {
            this->_tbl.insert (il);
        }

        const type_desc *find (JSON::value const &desc) const
        {
            json_map::const_iterator got = this->_tbl.find(&desc);
            if (got == this->_tbl.end()) {
                return nullptr;
            }
            else {
                return got->second;
            }
        }

    };

  /********** class tydesc_factory members **********/

    tydesc_factory::tydesc_factory (bool longInt, bool doubleReal)
      : _longInt(longInt), _doubleReal(doubleReal), _tbl(new hash_table()),
        _bool(nullptr), _int(nullptr), _string(nullptr)
    { }

    tydesc_factory::~tydesc_factory () { delete this->_tbl; }

    const type_desc *tydesc_factory::mk_bool_tydesc ()
    {
        if (this->_bool == nullptr) {
            this->_bool = new bool_tydesc();
        }
        return this->_bool;
    }

    const type_desc *tydesc_factory::mk_int_tydesc ()
    {
        if (this->_int == nullptr) {
            if (this->_longInt) {
                this->_int = new int64_tydesc();
            } else {
                this->_int = new int32_tydesc();
            }
        }
        return this->_int;
    }

    const type_desc *tydesc_factory::mk_string_tydesc ()
    {
        if (this->_string == nullptr) {
            this->_string = new string_tydesc();
        }
        return this->_string;
    }

    const type_desc *tydesc_factory::mk_tydesc (JSON::value const &desc)
    {
        const type_desc *res = this->_tbl->find (desc);

        if (res == nullptr) {
            res = this->from_json (&desc);
            if (res != nullptr) {
                this->_tbl->insert ({{&desc, res}});
            }
        }

        return res;
    }

  // utility function to get an image/tensor shape from a JSON array
    static bool get_shape (JSON::array const *shape, std::vector<uint32_t> &res)
    {
        if (shape == nullptr) {
            return true;
        }
        for (int i = 0;  i < shape->length();  i++) {
            JSON::integer *n = (*shape)[i]->asInteger();
            if ((n == nullptr) || (n->intVal() > 1)) {
                res.push_back(n->intVal());
            }
            else {
              // error bad dimension
                return true;
            }
        }
        return false;
    }

    const type_desc *tydesc_factory::from_json (const JSON::value *desc)
    {
        const JSON::string *s;
        const JSON::object *obj;

        if ((s = desc->asConstString()) != nullptr) {
            if (s->val().compare("bool") == 0) {
                return this->mk_bool_tydesc ();
            }
            else if (s->val().compare("int") == 0) {
                return this->mk_int_tydesc ();
            }
            else if (s->val().compare("string") == 0) {
                return this->mk_string_tydesc ();
            }
            else {
              // error: unknown type name
                return nullptr;
            }
        }
        else if ((obj = desc->asConstObject()) != nullptr) {
          // a tensor, sequence, dynamic sequence, or image type
            if ((s = obj->fieldAsConstString("kind")) != nullptr) {
                if (s->val().compare("tensor") == 0) {
                    const JSON::array *shape = obj->fieldAsConstArray("shape");
                    std::vector<uint32_t> dims;
                    if (get_shape (shape, dims)) {
                      // error: missing or bogus shape
                        return nullptr;
                    }
                    if (this->_doubleReal) {
                        return new tensor64_tydesc (dims);
                    }
                    else {
                        return new tensor32_tydesc (dims);
                    }
                }
                else if (s->val().compare("image") == 0) {
                    const JSON::integer *dim = obj->fieldAsConstInteger("dimension");
                    if ((dim->intVal() < 1) | (3 < dim->intVal())) {
                        return nullptr;
                    }
                    const JSON::array *shape = obj->fieldAsConstArray("shape");
                    std::vector<uint32_t> dims;
                    if (get_shape (shape, dims)) {
                      // error: missing or bogus shape
                        return nullptr;
                    }
                    return new image_tydesc (static_cast<uint32_t>(dim->intVal()), dims);
                }
                else if (s->val().compare("seq") == 0) {
                    JSON::value *elemTy = (*obj)["elem-ty"];
                    const JSON::integer *size = obj->fieldAsConstInteger("size");
return nullptr;
                }
                else if (s->val().compare("dynseq") == 0) {
                    JSON::value *elemTy = (*obj)["elem-ty"];
                    const type_desc *td;
                    if ((elemTy == nullptr) || ((td = this->from_json(elemTy)) == nullptr)) {
                        return nullptr;
                    }
return nullptr;
                }
                else {
                  // error: unknown type name
                    return nullptr;
                }
            }
            else {
              // error: missing "kind" field
                return nullptr;
            }
        }
        else {
          // error: malformed JSON
            return nullptr;
        }
    }


}
