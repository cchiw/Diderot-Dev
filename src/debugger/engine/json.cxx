/*! \file json.cxx
 *
 * Code for reading JSON files.
 *
 * \author John Reppy
 */

/*
 * This code is loosely based on that of the SimpleJSON Library (http://mjpa.in/json)
 * by Mike Anchor.
 *
 * COPYRIGHT (c) 2015-2017 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include "json.hxx"

namespace JSON {

  /***** class value member functions *****/

    value::~value () { }

    const object *value::asConstObject () const
    {
        return dynamic_cast<const object *>(this);
    }
    object *value::asObject ()
    {
        return dynamic_cast<object *>(this);
    }

    const array *value::asConstArray () const
    {
        return dynamic_cast<const array *>(this);
    }
    array *value::asArray ()
    {
        return dynamic_cast<array *>(this);
    }

    const number *value::asConstNumber () const
    {
        return dynamic_cast<const number *>(this);
    }
    number *value::asNumber ()
    {
        return dynamic_cast<number *>(this);
    }

    const integer *value::asConstInteger () const
    {
        return dynamic_cast<const integer *>(this);
    }
    integer *value::asInteger ()
    {
        return dynamic_cast<integer *>(this);
    }

    const real *value::asConstReal () const
    {
        return dynamic_cast<const real *>(this);
    }
    real *value::asReal ()
    {
        return dynamic_cast<real *>(this);
    }

    const string *value::asConstString () const
    {
        return dynamic_cast<const string *>(this);
    }
    string *value::asString ()
    {
        return dynamic_cast<string *>(this);
    }

    const boolean *value::asConstBool () const
    {
        return dynamic_cast<const boolean *>(this);
    }
    boolean *value::asBool ()
    {
        return dynamic_cast<boolean *>(this);
    }

   /***** class object member functions *****/

    object::~object ()
    {
      // need to delete contents
    }

    void object::insert (std::string key, value *val)
    {
        this->_value.insert (std::pair<std::string, value *>(key, val));
    }

    value *object::operator[] (std::string key) const
    {
        std::map<std::string, value *>::const_iterator got = this->_value.find(key);
        if (got == this->_value.end())
            return nullptr;
        else
            return got->second;
    }

    std::string object::toString() { return std::string("<object>"); }

    size_t object::computeHash () const
    {
        std::hash<int32_t> hasher;
        size_t h = hasher(this->_value.size());
/* TODO: hash elements */
        return h;
    }

  /***** class array member functions *****/

    array::~array ()
    {
      // need to delete contents
    }

    std::string array::toString() { return std::string("<array>"); }

    size_t array::computeHash () const
    {
        std::hash<int32_t> hasher;
        size_t h = hasher(this->_value.size());
/* TODO: hash elements */
        return h;
    }

  /***** class number member functions *****/

    number::~number () { }

  /***** class integer member functions *****/

    integer::~integer () { }

    std::string integer::toString() { return std::string("<integer>"); }

    size_t integer::computeHash () const
    {
        std::hash<int64_t> hasher;
        return hasher(this->_value);
    }

  /***** class real member functions *****/

    real::~real () { }

    std::string real::toString() { return std::string("<real>"); }

    size_t real::computeHash () const
    {
        std::hash<double> hasher;
        return hasher(this->_value);
    }

  /***** class string member functions *****/

    string::~string () { }

    std::string string::toString () { return this->_value; }

    size_t string::computeHash () const
    {
        std::hash<std::string> hasher;
        return hasher(this->_value);
    }

  /***** class boolean member functions *****/

    boolean::~boolean () { }

    std::string boolean::toString()
    {
        if (this->_value) {
            return std::string("true");
        }
        else {
            return std::string("false");
        }
    }

    size_t boolean::computeHash () const
    {
        std::hash<bool> hasher;
        return hasher(this->_value);
    }

  /***** class null member functions *****/

    null::~null () { }

    std::string null::toString() { return std::string("null"); }

    size_t null::computeHash () const
    {
        return 17;
    }

} // namespace JSON
