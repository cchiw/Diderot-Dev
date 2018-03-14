/*! \file load-log-desc.cxx
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

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include "strings.h" // for strcasecmp
#include "event-desc.hxx"
#include "log-desc.hxx"
#include "json.hxx"

/* NOTE: this table should agree with the "alignAndSize" function in
 * src/logging/gen/event-sig.sml.
 */
static struct {
    int		szb;	// size of argument in bytes
    int		alignb;	// alignment restriction in bytes
}	ArgTyInfo[] = {
	{ 4, 4 },	// SELF
	{ 4, 4 },	// STRAND
	{ 4, 4 },	// INT
	{ 4, 4 },	// WORD
	{ 4, 4 },	// FLOAT
	{ 8, 8 },	// DOUBLE
	{ 8, 8 },	// NEW_ID
	{ 8, 8 },	// EVENT_ID
	/* no entries for STR0, ... */
    };

class LogFileDescLoader {
  public:
    LogFileDescLoader ()
    {
	this->_desc = 0;
	this->_nextId = 1;  // 0 is reserved for NoEvent
    }

    LogFileDesc *FileDesc () const { return this->_desc; }

    bool GetLogEventsFile (const JSON::Value *v);
    bool GetLogViewFile (const JSON::Value *v);

    EventDesc *NewEvent (const JSON::Value *v);
    Group *NewGroup (const JSON::Value *obj);

    EventDesc *GetEventByName (const JSON::Value *v);
    EventDesc *GetEventField (const JSON::Object *obj, std::string const &name);

    void Error (const char *fmt, ...);

  protected:
    LogFileDesc		*_desc;
    int			_nextId;

    bool _GetArgs (const JSON::Value *v, std::vector<ArgDesc> &ads);

};

inline std::string getString (const JSON::Object *obj, std::string const &fld, std::string const &cxt)
{
    const JSON::String *s = JSON::asString((*obj)[fld]);
    if (s == nullptr) {
	std::cerr << "Error: expected string field \"" << fld << "\" in " << cxt << "\n";
	exit (1);
    }
    return s->value();
}

/*! \brief process the "args" array of an event descriptor.
 *  \param v the JSON object that represents the array of argument descriptors.
 *  \return the argument descriptors.
 */
bool LogFileDescLoader::_GetArgs (const JSON::Value *v, std::vector<ArgDesc> &ads)
{
    unsigned int location = 12;  /* the argument area starts at byte 12 */
    unsigned int nextLoc;

    const JSON::Array *arr = v->asArray();
    if (arr == nullptr) {
	std::cerr << "expected argument array, but found " << v << "\n";
	return false;
    }

    ads.reserve(arr->length());

    for (int i = 0;  i < arr->length();  i++) {
	const JSON::Object *arg = (*arr)[i]->asObject();
	ArgDesc desc;
	if (arg == nullptr) {
	    std::cerr << "expected argument object, but found " << (*arr)[i] << "\n";
	    return false;
	}

      /* get the fields */

	desc.name = getString(arg, "name", "event descriptor");
	std::string tyStr = getString(arg, "ty", "event descriptor");
	const JSON::Number *loc = (*arg)["loc"]->asNumber();
	desc.desc = getString(arg, "desc", "event descriptor");

      /* translate the argument type */
	int n;
	if (strcasecmp(tyStr.c_str(), "self") == 0) desc.ty = SELF;
	else if (strcasecmp(tyStr.c_str(), "strand") == 0) desc.ty = STRAND;
	else if (strcasecmp(tyStr.c_str(), "int") == 0) desc.ty = INT;
	else if (strcasecmp(tyStr.c_str(), "word") == 0) desc.ty = WORD;
	else if (strcasecmp(tyStr.c_str(), "float") == 0) desc.ty = FLOAT;
	else if (strcasecmp(tyStr.c_str(), "double") == 0) desc.ty = DOUBLE;
	else if (strcasecmp(tyStr.c_str(), "new-id") == 0) desc.ty = NEW_ID;
	else if (strcasecmp(tyStr.c_str(), "id") == 0) desc.ty = EVENT_ID;
	else if (sscanf(tyStr.c_str(), "str%d", &n) == 1) desc.ty = (ArgType)((int)STR0 + n);
	else {
	    this->Error ("unrecognized argument type \"%s\" for field \"%s\"\n",
		tyStr.c_str(), desc.name.c_str());
	    return false;
	}

      /* compute the location (if not given) */
	int sz, align;
	if (desc.ty >= STR0) {
	    sz = desc.ty - STR0;
	    align = 1;
	}
	else {
	    sz = ArgTyInfo[desc.ty].szb;
	    align = ArgTyInfo[desc.ty].szb;
	}
	if (loc != nullptr) {
	    location = static_cast<int>(loc->value());
	}
	else {
	    location = (location + (align-1)) & ~(align-1);
	}
	desc.loc = location;
	nextLoc = location + sz;

	ads.push_back(desc);

	location = nextLoc;
    }

    return true;
}

/*! \brief construct a log-file description by reading in the JSON file.
 *  \param logDescFile the path to the log-events.json file
 *  \param logViewFile the path to the log-view.json file
 *  \return the log-file descriptor or 0 if there was an error.
 */
LogFileDesc *LoadLogDesc (std::string const &logDescFile, std::string const &logViewFile)
{
    LogFileDescLoader loader;
    JSON::Value *jVal = JSON::ParseFile (logDescFile);
    bool r1 = loader.GetLogEventsFile (jVal);
    if (r1
    && loader.GetLogViewFile (JSON::ParseFile (logViewFile)))
	return loader.FileDesc();
    else
	return 0;

}


/***** class LogFileDescLoader member functions *****/

bool LogFileDescLoader::GetLogEventsFile (const JSON::Value *val)
{
    const JSON::Object *v;

    if ((val == nullptr) || ((v = val->asObject()) == nullptr)) {
	return false;
    }

    std::string date = getString(v, "date", "log-events file");
    const JSON::Array *version = JSON::asArray((*v)["version"]);

    const JSON::Array *events = JSON::asArray((*v)["events"]);

    if (events == nullptr) return false;

  // allocate the events vector, including a slot for NoEvent
    this->_desc = new LogFileDesc ();
    std::vector<EventDesc *> *eds = &(this->_desc->_events);

  /* initialize the events array */
    eds->reserve (events->length() + 1);
    eds->push_back (new EventDesc()); /* NoEvent */
    for (unsigned int i = 0;  i < events->length();  i++) {
	EventDesc *ed = NewEvent ((*events)[i]);
	if (ed == nullptr) {
	    return false;
	}
	eds->push_back (ed);
    }

    return true;

}

bool LogFileDescLoader::GetLogViewFile (const JSON::Value *val)
{
    const JSON::Object *v;

    if ((val == nullptr) || ((v = val->asObject()) == nullptr)) {
	return false;
    }

    std::string date = getString(v, "date", "log-view file");
    const JSON::Array *version = JSON::asArray((*v)["version"]);
    const JSON::Object *root = JSON::asObject((*v)["root"]);

/* FIXME: we should check consistency between the log-events file
 * and the log-view file.
 */

    if ((version == nullptr) || (root == nullptr)) {
	return false;
    }

    Group *grp = this->NewGroup (root);
    if ((grp == 0) || (grp->Kind() != EVENT_GROUP))
	return false;
    this->_desc->_root = dynamic_cast<EventGroup *>(grp);

  // finish up by computing the per-event info
    this->_desc->_InitEventInfo ();

    return true;

}

EventDesc *LogFileDescLoader::NewEvent (const JSON::Value *val)
{
    const JSON::Object *v;

    if ((val == nullptr) || ((v = val->asObject()) == nullptr)) {
	return nullptr;
    }

    std::string name = getString(v, "name", "event");
    const JSON::Array *args = asArray((*v)["args"]);
    std::string desc = getString(v, "desc", "event");

    if (args == nullptr)
	return nullptr;

    std::vector<ArgDesc> ads;
    if (! this->_GetArgs(args, ads)) {
	this->Error("bad argument for event %s\n", name.c_str());
	return nullptr;
    }

  /* get "is-src" attribute field */
    EventAttrs_t attributes;
    const JSON::Bool *isSrc = JSON::asBool((*v)["is-src"]);
    if ((isSrc == nullptr) || (! isSrc->value())) {
	attributes = ATTR_NONE;
    }
    else {
	attributes = ATTR_SRC;
    }

    EventDesc *ed = new EventDesc (name, this->_nextId++, attributes, ads, desc);

    return ed;

}

Group *LogFileDescLoader::NewGroup (const JSON::Value *val)
{
    const JSON::Object *v;

    if ((val == nullptr) || ((v = val->asObject()) == nullptr)) {
	return nullptr;
    }

    std::string desc = getString(v, "desc", "group");
    std::string kindStr = getString(v, "kind", "group");

    if (strcasecmp(kindStr.c_str(),"group") == 0) {
	const JSON::Array *events = JSON::asArray((*v)["events"]);
	const JSON::Array *groups = JSON::asArray((*v)["groups"]);
	if ((events == nullptr) || (groups == nullptr))
	    return nullptr;
	EventGroup *grp = new EventGroup (desc, events->length(), groups->length());
      /* add events to the group */
	for (int i = 0;  i < events->length();  i++) {
	    EventDesc *evt = this->GetEventByName ((*events)[i]);
	    if (evt == nullptr)
		return nullptr;
	    grp->AddEvent (i, evt);
	}
      /* add sub-groups to the group */
	for (int i = 0;  i < groups->length();  i++) {
	    Group *subgrp = this->NewGroup((*groups)[i]);
	    if (subgrp == nullptr)
		return nullptr;
	    grp->AddGroup (i, subgrp);
	}
	return grp;
    }
    else if (strcasecmp(kindStr.c_str(),"state") == 0) {
	std::string start = getString(v, "start", "state group");
	const JSON::Array *states = JSON::asArray((*v)["states"]);
	const JSON::Array *colors = JSON::asArray((*v)["colors"]);
	const JSON::Array *trans = JSON::asArray((*v)["transitions"]);
	if ((states == nullptr) || (trans == nullptr)) {
	    return nullptr;
	}
	int nStates = states->length();
	if ((colors != nullptr) && ((colors->isArray() || (colors->asArray()->length() != nStates))))
	    return nullptr;
	int nColors = (colors == nullptr) ? 0 : colors->asArray()->length();
	int nTrans = trans->length();
	if ((nStates == 0) || (nTrans == 0)) {
	    return nullptr;
	}
	StateGroup *grp = new StateGroup (desc, nStates, nTrans);
      /* add the state names */
	for (int i = 0;  i < nStates;  i++) {
	    const JSON::String *st = JSON::asString((*states)[i]);
	    const JSON::String *color = (colors == nullptr) ? nullptr : JSON::asString((*colors)[i]);
	    if (st == nullptr) {
		return nullptr;
	    }
	    grp->AddState(i, st->value(), (color == nullptr) ? nullptr : color->value());
	}
      /* add the transitions */
	for (int i = 0;  i < nTrans;  i++) {
	  /* add transitions and mark events as being in a state group */
	    const JSON::Array *t = JSON::asArray((*trans)[i]);
	    if ((t == nullptr) || (t->length() != 2)) {
		return nullptr;
	    }
	    EventDesc *evt = this->GetEventByName ((*t)[0]);
	    const JSON::String *stName = JSON::asString((*t)[1]);
	    if ((evt == nullptr) || (stName == nullptr))
		return nullptr;
	    grp->AddTransition (i, evt, stName->value());
	    evt->SetAttr (ATTR_STATE);
	}
      /* record the start state */
	grp->SetStart (start);
	return grp;
    }
    else if (strcasecmp(kindStr.c_str(), "interval") == 0) {
	EventDesc *a = this->GetEventField (v, "start");
	EventDesc *b = this->GetEventField (v, "end");
	std::string color = getString(v, "color", "interval group");
	if ((a == 0) || (b == 0))
	    return 0;
	else {
	    a->SetAttr (ATTR_INTERVAL);
	    b->SetAttr (ATTR_INTERVAL);
	    return new IntervalGroup (desc, a, b, color);
	}
    }
    else if (strcasecmp(kindStr.c_str(),"dependent") == 0) {
	EventDesc *src = this->GetEventField (v, "src");
	EventDesc *dst = this->GetEventField (v, "dst");
	std::string color = getString(v, "color", "dependent group");
	if ((src == 0) || (dst == 0))
	    return 0;
	else {
	    src->SetAttr (ATTR_DEPENDENT);
	    dst->SetAttr (ATTR_DEPENDENT);
	    return new DependentGroup (desc, src, dst, color);
	}
    }
    else {
	this->Error("bad group kind %s\n", kindStr.c_str());
	return 0;
    }

}

EventDesc *LogFileDescLoader::GetEventByName (const JSON::Value *v)
{
    const JSON::String *evtName = JSON::asString(v);
    if (evtName == nullptr)
	return nullptr;
    EventDesc *ed = this->_desc->FindEventByName (evtName->value());
    if (ed == nullptr)
	this->Error ("unknown event \"%s\"\n", evtName->value().c_str());
    return ed;

}

EventDesc *LogFileDescLoader::GetEventField (const JSON::Object *obj, std::string const &name)
{
    const JSON::Value *fld = (*obj)[name];

    if (fld == nullptr) {
	this->Error ("unable to find field \"%s\" in JSON object\n", name.c_str());
	return nullptr;
    }
    else
	return this->GetEventByName (fld);

}

/* error reporting */
void LogFileDescLoader::Error (const char *fmt, ...)
{
    va_list va;
    va_start (va, fmt);
    vfprintf (stderr, fmt, va);
    va_end (va);
}
