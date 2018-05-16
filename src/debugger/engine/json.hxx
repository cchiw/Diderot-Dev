/*! \file json.hxx
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

#ifndef _JSON_HXX_
#define _JSON_HXX_

#include <vector>
#include <string>
#include <map>

namespace JSON {

  //! the types of JSON values
    enum json_type {
        T_OBJECT,       //!< object consisting of name-value pairs
        T_ARRAY,        //!< arrays of JSON values
        T_INTEGER,      //!< integer numbers (represented as int64_t)
        T_REAL,         //!< real numbers (represented as doubles)
        T_STRING,       //!< strings
        T_BOOL,         //!< booleans
        T_NULL          //!< the null value
    };

    class value;
    class object;
    class array;
    class number;
    class integer;
    class real;
    class string;
    class boolean;
    class null;

  // parse a JSON file; this returns nullptr if there is a parsing error
    value *parse_file (std::string filename);

  // virtual base class of JSON values
    class value {
      public:

      //! return the type of this JSON value
        json_type type() const { return this->_ty; }

      //! return true if this value is an object
        bool isObject() const { return (this->type() == T_OBJECT); }

      //! return true if this value is an array
        bool isArray() const { return (this->type() == T_ARRAY); }

      //! return true if this value is a number
        bool isNumber() const
        {
            return (this->type() == T_REAL) || (this->type() == T_INTEGER);
        }

      //! return true if this value is an integer
        bool isInteger() const { return (this->type() == T_INTEGER); }

      //! return true if this value is a real number
        bool isReal() const { return (this->type() == T_REAL); }

      //! return true if this value is a string
        bool isString() const { return (this->type() == T_STRING); }

      //! return true if this value is a boolean
        bool isBool() const { return (this->type() == T_BOOL); }

      //! return true if this value is the null value
        bool isNull() const { return (this->type() == T_NULL); }

      //! dynamic cast to JSON object
        const object *asConstObject () const;
        object *asObject ();

      //! dynamic cast to JSON array
        const array *asConstArray () const;
        array *asArray ();

      //! dynamic cast to JSON number
        const number *asConstNumber () const;
        number *asNumber ();

      //! dynamic cast to JSON integer
        const integer *asConstInteger () const;
        integer *asInteger ();

      //! dynamic cast to JSON real number
        const real *asConstReal () const;
        real *asReal ();

      //! dynamic cast to JSON string
        const string *asConstString () const;
        string *asString ();

      //! dynamic cast to JSON boolean
        const boolean *asConstBool () const;
        boolean *asBool ();

        size_t hash () const
        {
            if (this->_hashed) {
                return this->_hash;
            }
            else {
                return this->computeHash();
            }
        }

        virtual ~value();
        virtual std::string toString() = 0;

      protected:

        json_type _ty;
        mutable bool _hashed;
        mutable size_t _hash;

        explicit value (json_type ty) : _ty(ty), _hashed(false) { };

        virtual size_t computeHash () const = 0;

    };

    inline std::ostream& operator<< (std::ostream& s, value *v)
    {
        return s << v->toString();
    }

  //! JSON objects
    class object : public value {
      public:
        object () : value(T_OBJECT), _value() { };
        ~object ();

      //! return the number of fields in the object
        int size () const { return this->_value.size(); }

      //! insert a key-value pair into the object
        void insert (std::string key, value *val);

      //! return the value corresponding to the given key.
      //! \returns nullptr if the key is not defined in the object
        value *operator[] (std::string key) const;

      //! return an object-valued field
      //! \returns nullptr if the field is not present or is not an object
        object *fieldAsObject (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asObject() : nullptr;
        }
        const object *fieldAsConstObject (std::string key) const
        {
            const value *v = (*this)[key];
            return (v != nullptr) ? v->asConstObject() : nullptr;
        }

      //! return an array-valued field
      //! \returns nullptr if the field is not present or is not an array
        array *fieldAsArray (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asArray() : nullptr;
        }
        const array *fieldAsConstArray (std::string key) const
        {
            const value *v = (*this)[key];
            return (v != nullptr) ? v->asConstArray() : nullptr;
        }

      //! return a number-valued field
      //! \returns nullptr if the field is not present or is not a number
        number *fieldAsNumber (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asNumber() : nullptr;
        }

      //! return an integer-valued field
      //! \returns nullptr if the field is not present or is not an integer
        integer *fieldAsInteger (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asInteger() : nullptr;
        }
        const integer *fieldAsConstInteger (std::string key) const
        {
            const value *v = (*this)[key];
            return (v != nullptr) ? v->asConstInteger() : nullptr;
        }

      //! return an real-valued field
      //! \returns nullptr if the field is not present or is not a real
        real *fieldAsReal (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asReal() : nullptr;
        }

      //! return an string-valued field
      //! \returns nullptr if the field is not present or is not a string
        string *fieldAsString (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asString() : nullptr;
        }
        const string *fieldAsConstString (std::string key) const
        {
            const value *v = (*this)[key];
            return (v != nullptr) ? v->asConstString() : nullptr;
        }

      //! return an bool-valued field
      //! \returns nullptr if the field is not present or is not a bool
        boolean *fieldAsBool (std::string key)
        {
            value *v = (*this)[key];
            return (v != nullptr) ? v->asBool() : nullptr;
        }
        const boolean *fieldAsConstBool (std::string key) const
        {
            const value *v = (*this)[key];
            return (v != nullptr) ? v->asConstBool() : nullptr;
        }

        std::string toString();

      private:
        std::map<std::string, value *> _value;

        size_t computeHash () const;

    };

  //! JSON arrays
    class array : public value {
      public:
        array () : value(T_ARRAY), _value() { };
        ~array ();

        int length () const { return static_cast<int>(this->_value.size()); }
        void add (value *v) { this->_value.push_back(v); }

        value *operator[] (int idx) const { return this->_value[idx]; }

        std::string toString();

      private:
        std::vector<value *> _value;

        size_t computeHash () const;
    };

  //! base class for JSON numbers
    class number : public value {
      public:
        virtual ~number ();

        virtual std::string toString() = 0;
        virtual double realVal () const = 0;

      protected:
        explicit number (json_type ty) : value(ty) { };

        virtual size_t computeHash () const = 0;

    };

    class integer : public number {
      public:
        integer (int64_t v) : number(T_INTEGER), _value(v) { }
        ~integer ();

        int64_t intVal () const { return this->_value; }
        double realVal () const { return static_cast<double>(this->_value); }

        std::string toString();

      private:
        int64_t _value;

        size_t computeHash () const;
    };

    class real : public number {
      public:
        real (double v) : number(T_REAL), _value(v) { }
        ~real ();

        double realVal () const { return this->_value; }

        std::string toString();

      private:
        double _value;

        size_t computeHash () const;
    };

    class string : public value {
      public:
        string (std::string v) : value(T_STRING), _value(v) { };
        ~string ();

        std::string val () const { return this->_value; }

        std::string toString();

      private:
        std::string _value;

        size_t computeHash () const;
    };

    class boolean : public value {
      public:
        boolean (bool v) : value(T_BOOL), _value(v) { };
        ~boolean ();

        bool val () const { return this->_value; }

        std::string toString();

      private:
        bool _value;

        size_t computeHash () const;
    };

    class null : public value {
      public:
        null () : value(T_NULL) { };
        ~null ();

        std::string toString();

      private:
        size_t computeHash () const;

    };

} // namespace JSON

#endif // !_JSON_HXX_
