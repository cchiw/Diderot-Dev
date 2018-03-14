/*! \file log-desc.cxx
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
#include "log-desc.hxx"
#include <iostream> /* FIXME: replace error message in AddTransition with something else */
#include <cstdlib> /* FIXME: replace exit in AddTransition with something else */
#include <stack>
#include <assert.h>

/* additional info about events */
struct EventGrpInfo {
    std::vector<StateGroup *>		*stateGrps;
    std::vector<IntervalGroup *>	*intervalGrps;
    std::vector<DependentGroup *>	*dependentGrps;
};

/***** class Group member functions *****/

Group::Group (std::string const &desc, GroupKind kind)
{
    this->_desc = desc;
    this->_kind = kind;
}

Group::~Group ()
{ }

bool Group::containsEvent (EventDesc *) const
{
    return false;
}

/***** class EventGroup member functions *****/

EventGroup::EventGroup (std::string const &desc, int nEvents, int nGroups)
    : Group (desc, EVENT_GROUP),
	_events(nEvents, (EventDesc *)0),
	_groups(nGroups, (Group *)0)
{
}

EventGroup::~EventGroup ()
{
}

bool EventGroup::containsEvent (EventDesc *evt) const
{
  /* check the list of events */
    for (unsigned int i = 0;  i < this->_events.size();  i++) {
	if (this->_events[i] == evt)
	    return true;
    }
  /* recursively check sub-groups */
    for (unsigned int i = 0;  i < this->_groups.size();  i++) {
	if (this->_groups[i]->containsEvent(evt))
	    return true;
    }
  /* otherwise, not in this group */
    return false;
}

void EventGroup::AddEvent (int i, EventDesc *item)
{
    this->_events.at(i) = item;
}

void EventGroup::AddGroup (int i, Group *item)
{
    this->_groups.at(i) = item;
    item->SetGroup (this);
}


/***** class StateGroup member functions *****/

StateGroup::StateGroup (std::string const &desc, int nStates, int nTransitions)
    : Group (desc, STATE_GROUP),
	_stateNames(nStates, ""),
	_stateColors(nStates, ""),
	_transitions(nTransitions, StateTransition()),
	_events()
{ }

StateGroup::~StateGroup ()
{ }

bool StateGroup::containsEvent (EventDesc *evt) const
{
    for (unsigned int i = 0;  i < this->_transitions.size();  i++) {
	if (this->_transitions.at(i)._event == evt)
	    return true;
    }
    return false;
}

int StateGroup::NextState (int st, EventDesc *evt) const
{
    for (unsigned int i = 0;  i < this->_transitions.size();  i++) {
	if (this->_transitions.at(i)._event == evt)
	    return this->_transitions.at(i)._nextState;
    }
    return -1;
}

void StateGroup::SetStart (std::string const &st)
{
  // first map the state name to an index
    unsigned int state;
    for (state = 0;  state < this->_stateNames.size();  state++) {
	if (st.compare(this->_stateNames.at(state)) == 0)
	    break;
    }
    if (state == this->_stateNames.size()) {
	std::cerr << "unknown state name \"" << st << "\"\n";
	exit(1);
    }

    this->_start = state;

}

void StateGroup::AddState (int i, std::string const &st, std::string const &color)
{
    this->_stateNames.at(i) = st;
    this->_stateColors.at(i) = color;
}

void StateGroup::AddTransition (int i, EventDesc *evt, std::string const &st)
{
  // first map the state name to an index
    unsigned int state;
    for (state = 0;  state < this->_stateNames.size();  state++) {
	if (st.compare(this->_stateNames.at(state)) == 0)
	    break;
    }
    if (state == this->_stateNames.size()) {
	std::cerr << "unknown state name \"" << st << "\"\n";
	exit(1);
    }

  // then add the transition info
    this->_transitions.at(i)._event = evt;
    this->_transitions.at(i)._nextState = state;

  // look for evt in the _events vector
    for (std::vector<EventDesc *>::iterator iter = this->_events.begin();
	iter < this->_events.end();  iter++
    ) {
	if ((*iter)->Id() == evt->Id())
	    return; /* evt is already in the _events vector */
    }

  // if we get here, then evt should be added to the _events vector
    this->_events.push_back(evt);

}

/***** class IntervalGroup member functions *****/

IntervalGroup::IntervalGroup (std::string const &desc, EventDesc *a, EventDesc *b, std::string const &color)
    : Group (desc, INTERVAL_GROUP), _start(a), _end(b), _color(color)
{ }

IntervalGroup::~IntervalGroup ()
{ }

bool IntervalGroup::containsEvent (EventDesc *evt) const
{
    return (this->_start == evt) || (this->_end == evt);
}


/***** class DependentGroup member functions *****/

DependentGroup::DependentGroup (std::string const &desc, EventDesc *src, EventDesc *dst, std::string const &color)
    : Group (desc, DEPENDENT_GROUP), _src(src), _dst(dst), _color(color)
{ }

DependentGroup::~DependentGroup ()
{ }

bool DependentGroup::containsEvent (EventDesc *evt) const
{
    return (this->_src == evt) || (this->_dst == evt);
}


/***** class LogFileDesc member functions *****/

LogFileDesc::LogFileDesc ()
    : _root(nullptr)
{ }

LogFileDesc::~LogFileDesc ()
{
    delete this->_root;
}

EventDesc *LogFileDesc::FindEventByName (std::string const &name) const
{
    for (auto it = this->_events.begin(); it != this->_events.end();  ++it) {
	if (name.compare((*it)->Name()) == 0)
	    return *it;
    }
    return 0;
}

std::vector<StateGroup *> *LogFileDesc::StateGroups (EventDesc *ed) const
{
    if (ed->HasAttr (ATTR_STATE))
	return this->_info[ed->Id()]->stateGrps;
    else
	return 0;
}

std::vector<IntervalGroup *> *LogFileDesc::IntervalGroups (EventDesc *ed) const
{
    if (ed->HasAttr (ATTR_INTERVAL))
	return this->_info[ed->Id()]->intervalGrps;
    else
	return 0;
}

std::vector<DependentGroup *> *LogFileDesc::DependentGroups (EventDesc *ed) const
{
    if (ed->HasAttr (ATTR_DEPENDENT))
	return this->_info[ed->Id()]->dependentGrps;
    else
	return 0;
}

/* add a group to a group vector */
template <class T>
static void AddGroup (std::vector<T *> *&v, T *g)
{
    if (v == 0)
	v = new std::vector<T *>(1, g);
    else
	v->push_back(g);
}

void LogFileDesc::_InitEventInfo ()
{
    class Visitor : public LogDescVisitor {
      public:
	Visitor (LogFileDesc *ld, std::vector<EventGrpInfo *> &info)
	    : _logFile(ld), _info(info)
	{ }

	void VisitGroup (EventGroup *) { }
	void VisitStateGroup (StateGroup *grp)
	{
	    std::vector<EventDesc *>::iterator iter;
	    std::vector<EventDesc *> evts = grp->Events();
	    for (iter = evts.begin();  iter < evts.end();  iter++) {
		EventDesc *ed = *iter;
		assert (ed != 0);
		AddGroup<StateGroup> (this->_info[ed->Id()]->stateGrps, grp);
	    }
	}
	void VisitIntervalGroup (IntervalGroup *grp)
	{
	    int id = grp->Start()->Id();
	    AddGroup<IntervalGroup> (this->_info[id]->intervalGrps, grp);
	    id = grp->End()->Id();
	    AddGroup<IntervalGroup> (this->_info[id]->intervalGrps, grp);
	}
	void VisitDependentGroup (DependentGroup *grp)
	{
	    int id = grp->Src()->Id();
	    AddGroup<DependentGroup> (this->_info[id]->dependentGrps, grp);
	    id = grp->Dst()->Id();
	    AddGroup<DependentGroup> (this->_info[id]->dependentGrps, grp);
	}
      private:
	std::vector<EventGrpInfo *> _info;
	LogFileDesc *_logFile;

    };

    this->_info.reserve(this->_events.size());
    for (auto it = this->_events.begin(); it != this->_events.end(); ++it) {
	if ((*it)->isSimpleEvent()) {
	    this->_info.push_back(nullptr);
	}
	else {
	    this->_info.push_back(new EventGrpInfo);
	}
    }

    Visitor v(this, this->_info);
    this->PreOrderWalk (&v);

}

/* visitor walks of the event hierarchy */

struct StkNode {
    EventGroup	*grp;
    int		i;		// child index in the group

    StkNode (EventGroup *g) { this->grp = g; this->i = 0; }

    Group *Next ()
    {
	if (this->i < grp->NumKids())
	    return grp->Kid(this->i++);
	else
	    return 0;
    }

};

typedef std::stack<StkNode> Stack_t;

//! \brief do a pre-order traversal of the event hierarchy, calling the visitor methods at each
//! node.
void LogFileDesc::PreOrderWalk (LogDescVisitor *visitor)
{
    Stack_t stk;

    visitor->VisitGroup (this->_root);
    stk.push (StkNode(this->_root));
    while (! stk.empty()) {
	Group *p = stk.top().Next();
	if (p == 0) {
	    stk.pop();
	}
	else switch (p->Kind()) {
	  case EVENT_GROUP: {
		EventGroup *grp = reinterpret_cast<EventGroup *>(p);
		visitor->VisitGroup (grp);
		stk.push (StkNode(grp));
	    } break;
	  case STATE_GROUP: {
		StateGroup *grp = reinterpret_cast<StateGroup *>(p);
		visitor->VisitStateGroup (grp);
	    } break;
	  case INTERVAL_GROUP: {
		IntervalGroup *grp = reinterpret_cast<IntervalGroup *>(p);
		visitor->VisitIntervalGroup (grp);
	    } break;
	  case DEPENDENT_GROUP: {
		DependentGroup *grp = reinterpret_cast<DependentGroup *>(p);
		visitor->VisitDependentGroup (grp);
	    } break;
	}
    }
}

//! \brief do a post-order traversal of the event hierarchy, calling the visitor methods at each
//! node.
void LogFileDesc::PostOrderWalk (LogDescVisitor *visitor)
{
#ifdef FIXME
    Stack_t stk;

    stk.push (StkNode(this->_root));
    while (! stk.empty()) {
	Group *p = stk.top().Next();
	if (p == 0) {
	    visitor->VisitGroup (stk.top().grp);
	    stk.pop();
	}
	else {
	    EventGroup *grp = dynamic_cast<EventGroup *>(p);
	    if (grp != 0) {
		visitor->VisitGroup (grp);
		stk.push (StkNode(grp));
   	    }
	    else
		visitor->VisitEvent (static_cast<EventDesc *>(p));
	}
    }
#endif
}
