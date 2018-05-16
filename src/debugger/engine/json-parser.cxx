/*! \file json-parser.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#include "json.hxx"
#include <iostream>
#include <fstream>
#include <cctype>
#include "strings.h"

namespace JSON {

class in_buffer {
  public:
    in_buffer (std::string filename);
    ~in_buffer () { delete this->_buffer; }

    in_buffer const &operator++ (int _unused) {
        if (this->_buffer[this->_i++] == '\n') this->_lnum++;
        return *this;
    }
    in_buffer const &operator += (int n) { this->_i += n;  return *this; }
    const char *operator() () const { return &(this->_buffer[this->_i]); }
    char operator[] (int j) const { return this->_buffer[this->_i + j]; }
    char operator* () const { return this->_buffer[this->_i]; }
    int avail () const { return this->_len - this->_i; }
    bool eof () const { return this->_i >= this->_len; }

    void error (std::string msg)
    {
#ifndef NDEBUG
        std::cerr << "JSON::parse_file(" << this->_file << "): " << msg
            << " at line " << this->_lnum << std::endl;
        std::cerr << "    input = \"";
        int n = this->avail();
        if (20 < n) n = 20;
        for (int i = 0;  (i < 20);  i++) {
            if (isprint(this->_buffer[this->_i+i]))
                std::cerr << this->_buffer[this->_i+i];
            else
                std::cerr << ".";
        }
        std::cerr << " ...\n" << std::endl;
#endif
    }

  private:
    std::string _file;
    char        *_buffer;
    int         _i;     // character index
    int         _lnum;  // current line number
    int         _len;   // buffer size

};

in_buffer::in_buffer (std::string filename)
    : _file(filename), _buffer(nullptr), _i(0), _lnum(0), _len(0)
{
  // open the JSON file for reading
    std::ifstream inS(filename, std::ios::in);
    if (inS.fail())
        return;

  // figure out the size of the file
    inS.seekg (0, inS.end);
    int length = inS.tellg();
    inS.seekg (0, inS.beg);

  // read length bytes
    this->_lnum = 1;
    this->_buffer = new char[length];
    inS.read (this->_buffer, length);

    if (inS.fail()) {
        delete this->_buffer;
        this->_buffer = nullptr;
        return;
    }

    this->_len = length;
}

// forward decls
static bool skip_ws (in_buffer &datap);
static value *parse (in_buffer &datap);

// parse a JSON file; this returns nullptr if there is a parsing error
value *parse_file (std::string filename)
{
  // open the JSON file for reading
    in_buffer datap(filename);
    if (datap.eof()) {
#ifndef NDEBUG
        std::cerr << "JSON::parse_file: unable to read \"" << filename << "\"" << std::endl;
#endif
        return nullptr;
    }

    if (! skip_ws (datap)) {
        return nullptr;
    }

    value *value = parse (datap);

    return value;

}

static bool skip_ws (in_buffer &datap)
{
    while ((! datap.eof()) && isspace(*datap))
        datap++;

    if (datap.eof()) {
        datap.error("unexpected eof");
        return false;
    }
    else
        return true;
}

static bool ExtractString (in_buffer &datap, std::string &str)
{
    str = "";

    if (*datap != '\"')
        return false;
    datap++;

    while (! datap.eof()) {
        // Save the char so we can change it if need be
        char nextChar = *datap;

        // Escaping something?
        if (nextChar == '\\') {
            // Move over the escape char
            datap++;
            // Deal with the escaped char
            switch (*datap) {
                case '"': nextChar = '"'; break;
                case '\\': nextChar = '\\'; break;
                case '/': nextChar = '/'; break;
                case 'b': nextChar = '\b'; break;
                case 'f': nextChar = '\f'; break;
                case 'n': nextChar = '\n'; break;
                case 'r': nextChar = '\r'; break;
                case 't': nextChar = '\t'; break;
                case 'u': /* no UNICODE support */
                // By the spec, only the above cases are allowed
                default:
                    datap.error("invalid escape sequence in string");
                    return false;
            }
        }
      // End of the string?
        else if (nextChar == '"') {
            datap++;
            str.reserve(); // Remove unused capacity
            return true;
        }
      // Disallowed char?
        else if (! isprint(nextChar) && (nextChar != '\t')) {
          // SPEC Violation: Allow tabs due to real world cases
            datap.error("invalid character in string");
            return false;
        }
      // Add the next char
        str += nextChar;
      // Move on
        datap++;
    }

  // If we're here, the string ended incorrectly
    return false;
}

static int64_t ParseInt (in_buffer &datap)
{
    int64_t n = 0;
    while (*datap != 0 && isdigit(*datap)) {
        n = n * 10 + (*datap - '0');
        datap++;
    }

    return n;
}

static double ParseDecimal (in_buffer &datap)
{
    double decimal = 0.0;
    double factor = 0.1;

    while ((! datap.eof()) && isdigit(*datap)) {
        int digit = (*datap - '0');
        decimal = decimal + digit * factor;
        factor *= 0.1;
        datap++;
    }
    return decimal;
}

static value *parse (in_buffer &datap)
{
    if (datap.eof()) {
        datap.error("unexpected end of file");
        return nullptr;
    }

  // Is it a string?
    if (*datap == '"') {
        std::string str;
        if (! ExtractString(datap, str))
            return nullptr;
        else
            return new string(str);
    }
  // Is it a boolean?
    else if ((datap.avail() >= 4) && strncasecmp(datap(), "true", 4) == 0) {
        datap += 4;
        return new boolean(true);
    }
    else if ((datap.avail() >=  5) && strncasecmp(datap(), "false", 5) == 0) {
        datap += 5;
        return new boolean(false);
    }
  // Is it a null?
    else if ((datap.avail() >=  4) && strncasecmp(datap(), "null", 4) == 0) {
        datap += 4;
        return new null();
    }
  // Is it a number?
    else if (*datap == '-' || isdigit(*datap)) {
      // Negative?
        bool neg = *datap == '-';
        bool isReal = false;
        if (neg) datap++;

        int64_t whole = 0;

      // parse the whole part of the number - only if it wasn't 0
        if (*datap == '0')
            datap++;
        else if (isdigit(*datap))
            whole = ParseInt(datap);
        else {
            datap.error("invalid number");
            return nullptr;
        }

        double r;

      // Could be a decimal now...
        if (*datap == '.') {
            r = (double)whole;
            isReal = true;
            datap++;

            // Not get any digits?
            if (! isdigit(*datap)) {
                datap.error("invalid number");
                return nullptr;
            }

            // Find the decimal and sort the decimal place out
            // Use ParseDecimal as ParseInt won't work with decimals less than 0.1
            // thanks to Javier Abadia for the report & fix
            double decimal = ParseDecimal(datap);

            // Save the number
            r += decimal;
        }

        // Could be an exponent now...
        if (*datap == 'E' || *datap == 'e') {
            if (!isReal) {
                r = (double)whole;
                isReal = true;
            }
            datap++;

            // Check signage of expo
            bool neg_expo = false;
            if (*datap == '-' || *datap == '+') {
                neg_expo = *datap == '-';
                datap++;
            }

            // Not get any digits?
            if (! isdigit(*datap)) {
                datap.error("invalid number");
                return nullptr;
            }

            // Sort the expo out
            double expo = ParseInt(datap);
            for (double i = 0.0; i < expo; i++)
                r = neg_expo ? (r / 10.0) : (r * 10.0);
        }

        if (isReal) {
            return new real (neg ? -r : r);
        }
        else {
            return new integer (neg ? -whole : whole);
        }

    }
  // An object?
    else if (*datap == '{') {
        object *obj = new object();

        datap++;

        while (!datap.eof()) {
          // Whitespace at the start?
            if (! skip_ws(datap)) {
                delete obj;
                return nullptr;
            }

          // Special case: empty object
            if ((obj->size() == 0) && (*datap == '}')) {
                datap++;
                return obj;
            }

          // We want a string now...
            std::string name;
// CHECK: do we need to look for "?
            if (! ExtractString(datap, name)) {
                datap.error("expected label");
                delete obj;
                return nullptr;
            }

          // More whitespace?
            if (! skip_ws(datap)) {
                delete obj;
                return nullptr;
            }

          // Need a : now
            if (*datap != ':') {
                datap.error("expected ':'");
                delete obj;
                return nullptr;
            }
            datap++;

          // More whitespace?
            if (! skip_ws(datap)) {
                delete obj;
                return nullptr;
            }

          // The value is here
            value *value = parse(datap);
            if (value == nullptr) {
                delete obj;
                return nullptr;
            }

          // Add the name:value
            obj->insert(name, value);

          // More whitespace?
            if (! skip_ws(datap)) {
                delete obj;
                return nullptr;
            }

            // End of object?
            if (*datap == '}') {
                datap++;
                return obj;
            }

            // Want a , now
            if (*datap != ',') {
                datap.error("expected ','");
                delete obj;
                return nullptr;
            }

            datap++;
        }

      // Only here if we ran out of data
        datap.error("unexpected eof");
        delete obj;
        return nullptr;
    }

    // An array?
    else if (*datap == '[') {
        array *arr = new array();

        datap++;

        while (! datap.eof()) {
          // Whitespace at the start?
            if (! skip_ws(datap)) {
                delete arr;
                return nullptr;
            }

          // Special case - empty array
            if ((arr->length() == 0) && (*datap == ']')) {
                datap++;
                return arr;
            }

          // Get the value
            value *value = parse(datap);
            if (value == nullptr) {
                delete arr;
                return nullptr;
            }

          // Add the value
            arr->add(value);

          // More whitespace?
            if (! skip_ws(datap)) {
                delete arr;
                return nullptr;
            }

          // End of array?
            if (*datap == ']') {
                datap++;
                return arr;
            }

          // Want a , now
            if (*datap != ',') {
                datap.error("expected ','");
                delete arr;
                return nullptr;
            }

            datap++;
        }

      // Only here if we ran out of data
        datap.error("unexpected eof");
        delete arr;
        return nullptr;
    }
  // Ran out of possibilites, it's bad!
    else {
        datap.error("bogus input");
        return nullptr;
    }
}

} // namespace JSON
