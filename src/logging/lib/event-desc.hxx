/*! \file event-desc.hxx
 *
 * \author John Reppy
 *
 * \brief In-memory representation of event descriptions as defined in
 * the log-events.json file.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * This code was ported from The Manticore Project (http://manticore.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _EVENT_DESC_HXX_
#define _EVENT_DESC_HXX_

#include "diderot/log-file.hxx"
#include <cstdint>
#include <string>
#include <vector>

/*! \brief event attributes; these include the "attrs" field from the
 *  log-events.json file as well as information about how the event is
 *  used in the log-view.json file.
 */
enum EventAttr {
    ATTR_NONE		= 0,		//!< no attribute
    ATTR_SRC		= (1 << 0),	//!< has "is-src" field set to true
    ATTR_STATE		= (1 << 1),	//!< member of state group
    ATTR_INTERVAL	= (1 << 2),	//!< member of interval group
    ATTR_DEPENDENT	= (1 << 3)	//!< member of dependent group
};

//! \brief bitmask for group-membership attributes
#define IN_GROUP_MASK	(ATTR_STATE | ATTR_INTERVAL | ATTR_DEPENDENT)

//! \brief bitmask of event attributes
typedef uint32_t EventAttrs_t;

/*! \brief the different types of event-argument data */
enum ArgType {
    SELF,		//!< strand's ID (32 bits)
    STRAND,             //!< another strand's ID (32 bits)
    INT,		//!< 32-bit signed integer data
    WORD,		//!< 32-bit unsigned integer data
    FLOAT,		//!< 32-bit floating-point data
    DOUBLE,		//!< 64-bit floating-point data
    NEW_ID,		//!< new unique event ID (64-bits)
    EVENT_ID,		//!< event ID (64-bits)
    STR0		//!< base type for fixed-length strings
};

#define STR(n)		(STR0+(n))
#define isSTR(ty)	((ty) > STR0)
#define STRLEN(ty)	((int)(ty) - (int)STR0)
#define MAX_STRLEN	20

//! \brief the static description of an event argument.
struct ArgDesc {
    std::string	name;		//!< the argument's name
    ArgType	ty;		//!< the argument's type
    int		loc;		//!< the offset of the field from the start
				//!  of the event (in bytes)
    std::string	desc;		//!< description of event
};

//! \brief An event-argument value
union ArgValue {
    uint32_t		sid;	//!< for strand IDs
    int32_t		i;
    uint32_t		w;
    float		f;
    double		d;
    uint64_t		id;
    char		str[MAX_STRLEN+1];
};

//! \brief An event-argument value tagged with its description
struct TaggedArgValue {
    const ArgDesc	*desc;	//!< the argument description
    ArgValue		val;	//!< the argument type
};

/*! \brief a description of an log-file event.
 */
class EventDesc {
  public:
  /// the name of this event
    std::string const &Name () const	{ return this->_name; }
  /// the integer tag for events of this type
    int Id () const { return this->_id; }
  /// the number of arguments
    int NArgs () const { return this->_args.size(); }
  /// the array of arguments; 0 if there are none
    const ArgDesc *Args () const { return this->_args.data(); }
  /// the event's description
    std::string const &Description () const { return this->_desc; }

  /// does an event have a given attribute?
    bool HasAttr (EventAttr attr) { return ((this->_attrs & (EventAttrs_t)attr) != 0); }
  /// mark that an event has a given attribute
    void SetAttr (EventAttr attr) { this->_attrs |= (EventAttrs_t)attr; }
  /// return true if the event is not a member of any group
    bool isSimpleEvent () const	{ return ((this->_attrs & IN_GROUP_MASK) == 0); }

    ArgDesc *GetArgDesc (int i)	{ return &(this->_args[i]); }
    ArgType GetArgType (int i)	{ return this->_args[i].ty; }

    ArgValue GetArg (diderot::log::event *evtData, int i);

    ~EventDesc ();

  protected:
    explicit EventDesc ();	/* NoEvent constructor */
    explicit EventDesc (
	std::string const &name,
	int id,
	EventAttrs_t attrs,
	std::vector<ArgDesc> const &args,
	std::string const &d);

    friend class LogFileDescLoader;

  private:
    std::string		_name;		/* the event's name */
    int			_id;
    std::vector<ArgDesc> _args;
    EventAttrs_t	_attrs;
    std::string		_desc;

};

#endif /* !_EVENT_DESC_HXX_ */

