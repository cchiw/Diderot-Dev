/*! \file event-desc.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * This code was ported from The Manticore Project (http://manticore.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include "event-desc.hxx"
#include <string.h>
#include <assert.h>

/***** class EventDesc member functions *****/

EventDesc::EventDesc ()
    : _name("NoEvent"), _id(0), _desc("not an event")
{ }

EventDesc::EventDesc (
	std::string const &name,
	int id,
	EventAttrs_t attrs,
	std::vector<ArgDesc> const &args,
	std::string const &d
    )
    : _name(name), _id(id), _args(args), _attrs(attrs), _desc(d)
{ }

EventDesc::~EventDesc ()
{ }

ArgValue EventDesc::GetArg (diderot::log::event *evtData, int i)
{
    assert ((0 <= i) && (i < this->_args.size()));

    ArgValue value;

    ArgType ty = this->_args[i].ty;
    void *p = (void *)((uint64_t)evtData + this->_args[i].loc);
    switch (ty) {
      case SELF:
	value.sid = *(uint32_t *)p;
	break;
      case STRAND:
	value.sid = *(uint32_t *)p;
	break;
      case INT:
	value.i = *(int32_t *)p;
	break;
      case WORD:
	value.w = *(uint32_t *)p;
	break;
      case FLOAT:
	value.f = *(float *)p;
	break;
      case DOUBLE:
	value.d = *(double *)p;
	break;
      case NEW_ID:
      case EVENT_ID:
	value.id = *(uint64_t *)p;
	break;
      default: {
	int len = STRLEN(ty);
	assert ((0 < len) && (len <= MAX_STRLEN));
	strncpy (value.str, (char *)p, len);
	value.str[len] = '\0';
	} break;
    }

    return value;
}


