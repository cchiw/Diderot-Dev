/*! \file svg.hxx
 *
 * \author John Reppy
 *
 * Infrastructure for generating information as SVG files.  The design of this
 * API was heavily influenced by the "Simple SVG" library (https://github.com/adishavit/simple-svg).
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _SVG_HXX_
#define _SVG_HXX_

#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
#include <initializer_list>

namespace svg {
    namespace __details {

	template <typename T>
	std::string attribute (
	    std::string const &name,
	    T const &value,
	    std::string const &unit = "")
	{
	    std::stringstream ss;
	    ss << name << "=\"" << value << unit << "\" ";
	    return ss.str();
	}
    }

    // Quick optional return type.  This allows functions to return an invalid
    //  value if no good return is possible.  The user checks for validity
    //  before using the returned value.
    template <typename T>
    class optional {
      public:
        optional<T> (T const &x) : _valid(true), _value(x) { }
        optional<T> () : _valid(false), _value(T()) { }
        T * operator-> ()
        {
            // If we try to access an invalid value, an exception is thrown.
            if (! this->_valid)
                throw std::exception();

            return &this->_value;
        }

        // Test for validity.
        bool operator! () const { return !this->_valid; }

      private:
        bool _valid;
        T _value;
    };

    struct Dimensions {
        double width;
        double height;
        Dimensions (double width, double height) : width(width), height(height) { }
        Dimensions (double combined = 0) : width(combined), height(combined) { }
    };

    struct Point {
        Point (double x = 0, double y = 0) : x(x), y(y) { }
        double x;
        double y;
    };

    // Defines the dimensions, origin, and origin offset of the document.
    struct Layout {
        enum Origin { TopLeft, BottomLeft, TopRight, BottomRight };

        Dimensions _dimensions;
        Origin _origin;
        Point _offset;

        Layout (
	    Dimensions const & dimensions,
	    Origin origin = TopLeft,
	    Point const &offset = Point(0, 0))
	  : _dimensions(dimensions), _origin(origin), _offset(offset)
	{ }

	double translateX (double x) const
	{
	    if (this->_origin == Layout::BottomRight || this->_origin == Layout::TopRight)
		return this->_dimensions.width - (x + this->_offset.x);
	    else
		return (this->_offset.x + x);
	}

	double translateY (double y) const
	{
	    if (this->_origin == Layout::BottomLeft || this->_origin == Layout::BottomRight)
		return this->_dimensions.height - (y + this->_offset.y);
	    else
		return (this->_offset.y + y);
	}

    }; // Layout

    class Serializable {
      public:
        std::string toString (Layout const &layout) {
	    std::stringstream ss;
	    this->serialize(ss, layout);
	    return ss.str();
	}

      protected:

        Serializable () { }
        virtual ~Serializable () { }

	virtual std::ostream &serialize (std::ostream &os, Layout const &layout) = 0;
    };

    class Attribute : public Serializable {
      public:
        virtual ~Attribute () { }

	virtual Attribute *copyToHeap () const = 0;

	std::ostream &serialize (std::ostream &os)
	{
	    os << this->_name << "=\"";
	    this->serializeValue(os);
	    return (os << "\"");
	}
	std::ostream &serialize (std::ostream &os, Layout const &layout)
	{
	    return this->serialize (os);
	}

      protected:
        std::string _name;

	virtual std::ostream &serializeValue (std::ostream &os) { return os; }

        Attribute (std::string const &name) : Serializable(), _name(name) { }
    };

    inline std::ostream &operator<< (std::ostream &os, Attribute &attr)
    {
	return attr.serialize(os);
    }

    class Color : public Serializable {
      public:
	Color () : _color("rgb(0,0,0)") { }
	Color (uint8_t r, uint8_t g, uint8_t b)
	{
	    std::stringstream ss;
	    ss << "rgb(" << (int)r << "," << (int)g << "," << (int)b << ")";
	    this->_color = ss.str();
	}
	Color (uint8_t gray)
	{
	    std::stringstream ss;
	    ss << "rgb(" << gray << "," << gray << "," << gray << ")";
	    this->_color =  ss.str();
	}
	Color (std::string name) : _color(name) {}
	Color (Color const &c) : _color(c._color) {}
	~Color () { }

	std::ostream &serialize (std::ostream &os)
	{
	    return os << this->_color;
	}
	std::ostream &serialize (std::ostream &os, Layout const &layout)
	{
	    return os << this->_color;
	}

      protected:
	std::string _color;
    };

    class Fill : public Attribute {
      public:
	Fill (Color const &c) : Attribute("fill"), _value(c) { }
	~Fill () { }
	Attribute *copyToHeap () const { return new Fill(this->_value); }
      protected:
	std::ostream &serializeValue (std::ostream &os) { return this->_value.serialize(os); }
      private:
	Color _value;
    };

    class FillOpacity : public Attribute {
      public:
	FillOpacity (double d = 1.0) : Attribute("fill-opacity"), _value(d) { }
	~FillOpacity () { }
	Attribute *copyToHeap () const { return new FillOpacity(this->_value); }
      protected:
	std::ostream &serializeValue (std::ostream &os) { return (os << this->_value); }
      private:
	double _value;
    };

    class Stroke : public Attribute {
      public:
	Stroke (Color const &c) : Attribute("stroke"), _value(c) { }
	~Stroke () { }
	Attribute *copyToHeap () const { return new Stroke(this->_value); }
      protected:
	std::ostream &serializeValue (std::ostream &os) { return this->_value.serialize(os); }
      private:
	Color _value;
    };

    class StrokeOpacity : public Attribute {
      public:
	StrokeOpacity (double d = 1.0) : Attribute("stroke-opacity"), _value(d) { }
	~StrokeOpacity () { }
	Attribute *copyToHeap () const { return new StrokeOpacity(this->_value); }
      protected:
	std::ostream &serializeValue (std::ostream &os) { return (os << this->_value); }
      private:
	double _value;
    };

  // elements include groups and shapes
    class Element : public Serializable {
      public:
	virtual std::ostream &serialize (std::ostream &os, Layout const &layout) { return os; }

      // add an attribute to an element
	void operator += (Attribute &attr) { this->_attrs.push_back(attr.copyToHeap()); }

	virtual ~Element () { }

	virtual Element *copyToHeap () const = 0;

      protected:
        std::string _name;
	std::vector<Attribute *> _attrs;

	Element (std::string const &name) : Serializable(), _name(name), _attrs() { }
	Element (std::string const &name, std::initializer_list<Attribute> init)
	  : Serializable(), _name(name), _attrs(init.size(), nullptr)
	{
	    for (auto it = init.begin();  it != init.end();  ++it) {
		this->_attrs.push_back(it->copyToHeap());
	    }
	}

        std::ostream &_openTag (std::ostream &os, Layout const & layout, bool isEmpty)
	{
	    os << "<" << this->_name;
	    for (auto it : this->_attrs) {
		os << " ";
		it->serialize (os, layout);
	    }
	    if (isEmpty) {
		os << "/>\n";
	    }
	    else {
		os << ">\n";
	    }
            return os;
	}

        std::ostream &_closeTag (std::ostream &os) {
	    return (os << "</" << this->_name << ">\n");
	}

    };

    class Group : public Element {
      public:
	Group () : Element("g") { }
/* problems with copy constructors
	Group (std::initializer_list<Element> init)
	  : Element("g"), _content(init.size(), nullptr)
	{
	    for (auto it = init.begin();  it != init.end();  ++it) {
		this->_content.push_back(it->copyToHeap());
	    }
	}
	Group (std::initializer_list<Attribute> attrs, std::initializer_list<Element> init)
	  : Element("g", attrs), _content(init.size(), nullptr)
	{
	    for (auto it = init.begin();  it != init.end();  ++it) {
		this->_content.push_back(it->copyToHeap());
	    }
	}
*/
	~Group () { }

	Element *copyToHeap () const
	{
	    Group *g = new Group ();
	    g->_attrs = this->_attrs;
	    g->_content = this->_content;
	    return g;
	}

        Group &operator<< (Element const & elem)
        {
            this->_content.push_back(elem.copyToHeap());
            return *this;
        }

        Group &operator<< (Attribute const & attr)
        {
            this->_attrs.push_back(attr.copyToHeap());
            return *this;
        }

	std::ostream &serialize (std::ostream &os, Layout const &layout)
	{
	    this->_openTag (os, layout, false);
            for (auto elem: this->_content) {
		elem->serialize (os, layout);
	    }
	    this->_closeTag (os);
	    return os;
	}

      protected:
	std::vector<Element *> _content;
    };

    class Shape : public Element {
      public:
	virtual std::ostream &serialize (std::ostream &os, Layout const &layout) = 0;
        virtual void offset (Point const & offset) = 0;

	virtual Element *copyToHeap () const = 0;

      protected:
        Shape (std::string const &name) : Element(name) { }
        virtual ~Shape() { }

    };

  // path commands
    namespace path {
	class Command {
	  public:
	    std::ostream &serialize (std::ostream &os, Layout const &layout)
	    {
		switch (this->_cmd) {
		  case CMD_M:
		    os << "M " << layout.translateX(this->_arg[0])
			<< "," << layout.translateY(this->_arg[1]);
		    break;
		  case CMD_m:
		    os << "m " << layout.translateX(this->_arg[0])
			<< "," << layout.translateY(this->_arg[1]);
		    break;
		  case CMD_L:
		    os << "L " << layout.translateX(this->_arg[0])
			<< "," << layout.translateY(this->_arg[1]);
		    break;
		  case CMD_l:
		    os << "l " << layout.translateX(this->_arg[0])
			<< "," << layout.translateY(this->_arg[1]);
		    break;
		  case CMD_Z:
		    os << "Z";
		    break;
	        }
		return os;
	    }
	    virtual ~Command () { }
	  protected:
	    enum Commands { CMD_M, CMD_m, CMD_L, CMD_l, CMD_Z };

	    Command::Commands	_cmd;
	    double		_arg[2];

	    Command (Command::Commands cmd) : _cmd(cmd), _arg{0, 0} { }
	    Command (Command::Commands cmd, double arg1) : _cmd(cmd), _arg{arg1, 0} { }
	    Command (Command::Commands cmd, double arg1, double arg2) : _cmd(cmd), _arg{arg1, arg2} { }

	};

	class M : public Command {
	  public:
	    M (double x, double y) : Command(Command::CMD_M, x, y) { }
	    M (Point const &pt) : Command(Command::CMD_M, pt.x, pt.y) { }
	};

	class m : public Command {
	  public:
	    m (double x, double y) : Command(Command::CMD_m, x, y) { }
	    m (Point const &pt) : Command(Command::CMD_m, pt.x, pt.y) { }
	};

	class L : public Command {
	  public:
	    L (double x, double y) : Command(Command::CMD_L, x, y) { }
	    L (Point const &pt) : Command(Command::CMD_L, pt.x, pt.y) { }
	};

	class l : public Command {
	  public:
	    l (double x, double y) : Command(Command::CMD_l, x, y) { }
	    l (Point const &pt) : Command(Command::CMD_l, pt.x, pt.y) { }
	};

	class Z : public Command {
	  public:
	    Z () : Command(Command::CMD_Z) { }
	};

    } // namespace path

    class Path : public Shape {
      public:

	Path () : Shape("path"), _d() { }
	Path (std::initializer_list<path::Command> init) : Shape("path"), _d(init) { }
	~Path () { }

	Path & operator<< (path::Command & cmd)
	{
	    this->_d.push_back(cmd);
	    return *this;
	}

	std::ostream &serialize (std::ostream &os, Layout const &layout)
	{
	    os << "<path";

	    for (auto it : _attrs) {
		os << " ";
		it->serialize (os, layout);
	    }

	    os << " d=\"";
	    bool first = true;
	    for (auto & cmd: this->_d) {
		if (first) {
		    first = false;
		}
		else {
		    os << " ";
		}
		cmd.serialize (os, layout);
	    }

	    os << "\"/>\n";

            return os;
	}

	Element *copyToHeap () const
	{
	    Path *p = new Path();
	    p->_attrs = this->_attrs;
	    p->_d = this->_d;
	    return p;
	}

	void offset (Point const & offset)
	{
/* TODO */
	}

      private:
	std::vector<path::Command> _d;
    };

    class Document {
      public:
        Document (Layout const &layout) : _layout(layout) { }
        Document (Dimensions const &dim) : _layout(Layout(dim)) { }

        Document &operator<< (Element & elem)
        {
            this->_content.push_back (elem.copyToHeap());
            return *this;
        }

	void serialize (std::ostream &os)
	{
            os << "<svg "
                << __details::attribute("width", this->_layout._dimensions.width, "px")
                << __details::attribute("height", this->_layout._dimensions.height, "px")
                << __details::attribute("xmlns", "http://www.w3.org/2000/svg")
                << __details::attribute("version", "1.1") << ">\n";
	    for (auto elem : this->_content) {
		elem->serialize (os, this->_layout);
	    }
	    os << "</svg>\n";
	}

        bool save (std::string const &file_name)
        {
            std::ofstream ofs(file_name);
            if (!ofs.good()) {
                return true;
	    }
	    this->serialize (ofs);
            ofs.close();
            return false;
        }

      private:
        Layout _layout;
	std::vector<Element *> _content;
    };

    void output_header (std::ostream &os)
    {
	os << "<?xml " << __details::attribute("version", "1.0")
	    << __details::attribute("encoding", "UTF-8")
	    << "?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" "
	    << "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
    }


}  // namespace svg

#endif //!_SVG_HXX_
