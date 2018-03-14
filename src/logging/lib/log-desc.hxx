/*! \file log-desc.hxx
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

#ifndef _LOG_DESC_HXX_
#define _LOG_DESC_HXX_

#include <vector>
#include <string>

class EventDesc;
struct EventGrpInfo;

//! \brief the different kinds of groups of events
enum GroupKind {
    EVENT_GROUP,	/*!< a group of events and/or groups */
    STATE_GROUP,	/*!< a group of events that represent state transitions */
    INTERVAL_GROUP,
    DEPENDENT_GROUP
};

/*! \brief the base of the group-class hierarchy */
class Group {
  public:
  /// the description of the group
    std::string const &Desc () const	{ return this->_desc; }
  /// the parent group; 0 for the root
    Group *Parent () const		{ return this->_grp; }
  /// the kind of the group
    GroupKind Kind() const		{ return this->_kind; }
  /// is this group the root?
    bool isRoot () const		{ return (this->_grp == 0); }

    void SetGroup (Group *grp) { this->_grp = grp; }

  /// Is the given event a member of this group?
    virtual bool containsEvent (EventDesc *evt) const;

  protected:
    std::string _desc;		/*!< the event's description */
    GroupKind	_kind;		/*!< the kind of group */
    Group	*_grp;		/*!< the group that this group belongs to */

    explicit Group (std::string const &desc, GroupKind kind);
    virtual ~Group ();

};

/*! \brief the representation of a group of events and/or groups */
class EventGroup : public Group {
  public:
    ~EventGroup ();

  /// Is the given event a member of this group?
    bool containsEvent (EventDesc *evt) const;

  /// return the number of events in this group
    int NumEvents () const { return this->_events.size(); }
  /// return the i'th event in this group
    EventDesc *Event (int i) const { return this->_events.at(i); }

  /// return the number of subgroups in this group
    int NumKids () const { return this->_groups.size(); }
  /// return the i'th subgroup in this group
    Group *Kid (int i) const { return this->_groups.at(i); }

  protected:
    EventGroup (std::string const &desc, int nEvents, int nGroups);

    void AddEvent (int i, EventDesc *item);
    void AddGroup (int i, Group *item);

  private:
    std::vector<EventDesc *>	_events;
    std::vector<Group *>	_groups;

    friend class LogFileDescLoader;

};

//! \brief state transitions in a state group
struct StateTransition {
    EventDesc	*_event;	//!< the event that causes the transition
    int		_nextState;	//!< the state being transitioned to.

    StateTransition () : _event(0), _nextState(0) { }
};

/*! \brief the representation of a state group */
class StateGroup : public Group {
  public:
    ~StateGroup ();

  /// Is the given event a member of this group?
    bool containsEvent (EventDesc *evt) const;

    int StartState () const { return this->_start; }
    int NextState (int st, EventDesc *evt) const;
    int NumStates () const { return this->_stateNames.size(); }

    std::string const &StateName (int i) const	{ return this->_stateNames.at(i); }
    std::string const &StateColor (int i) const { return this->_stateColors.at(i); }

    std::vector<EventDesc *> Events() const { return this->_events; }

  protected:
    StateGroup (std::string const &desc, int nStates, int nTransitions);

    void SetStart (std::string const &st);
    void AddState (int i, std::string const &st, std::string const &color);
    void AddTransition (int i, EventDesc *evt, std::string const &st);

  private:
    int				_start;		//!< the initial state
    std::vector<std::string>	_stateNames;	//!< the state names
    std::vector<std::string>	_stateColors;	//!< the state colors
    std::vector<StateTransition> _transitions;	//!< the transition table
    std::vector<EventDesc *>	_events;	//!< the events in the group

    friend class LogFileDescLoader;

};

/*! \brief the representation of an interval group */
class IntervalGroup : public Group {
  public:
    ~IntervalGroup ();

  /// Is the given event a member of this group?
    bool containsEvent (EventDesc *evt) const;

    EventDesc *Start () const		{ return this->_start; }
    EventDesc *End () const		{ return this->_end; }
    std::string const &Color () const	{ return this->_color; }

  protected:
    explicit IntervalGroup (std::string const &desc, EventDesc *a, EventDesc *b, std::string const &color);

  private:
    EventDesc	*_start;
    EventDesc	*_end;
    std::string _color;

    friend class LogFileDescLoader;

};

/*! \brief the representation of a group of dependent events */
class DependentGroup : public Group {
  public:
    ~DependentGroup ();

  /// Is the given event a member of this group?
    bool containsEvent (EventDesc *evt) const;

    EventDesc *Src () const		{ return this->_src; }
    EventDesc *Dst () const		{ return this->_dst; }
    std::string const &Color () const	{ return this->_color; }

  protected:
    explicit DependentGroup (std::string const &desc, EventDesc *src, EventDesc *dst, std::string const &color);

  private:
    EventDesc	*_src;
    EventDesc	*_dst;
    std::string _color;

    friend class LogFileDescLoader;

};

//! \brief abstract virtual class for traversing the group hierarchy
class LogDescVisitor {
  public:
    virtual void VisitGroup (EventGroup *grp) = 0;
    virtual void VisitStateGroup (StateGroup *grp) = 0;
    virtual void VisitIntervalGroup (IntervalGroup *grp) = 0;
    virtual void VisitDependentGroup (DependentGroup *grp) = 0;
};

/*! \brief the description of a log file.  This class combines the
 *  information from the log-events.json and event-view.json files.
 */
class LogFileDesc {
  public:
    EventGroup *Root () const { return this->_root; }
    int NumEventKinds () const { return this->_events.size(); }
    EventDesc *FindEventById (int id) const { return this->_events.at(id); }
    EventDesc *FindEventByName (std::string const &name) const;

  // methods for getting information about the groups that an
  // event belongs to.  These methods return 0 when the event
  // does not belong to any groups of the given kind.
    std::vector<StateGroup *> *StateGroups (EventDesc *) const;
    std::vector<IntervalGroup *> *IntervalGroups (EventDesc *) const;
    std::vector<DependentGroup *> *DependentGroups (EventDesc *) const;

  /* visitor walks of the event hierarchy */
    void PreOrderWalk (LogDescVisitor *visitor);
    void PostOrderWalk (LogDescVisitor *visitor);

    ~LogFileDesc ();

  protected:
    EventGroup			*_root;
    std::vector<EventDesc *>	_events;
    std::vector<EventGrpInfo *>	_info;

    LogFileDesc ();

    void _InitEventInfo ();

    friend class LogFileDescLoader;

};

extern LogFileDesc *LoadLogDesc (std::string const &logDescFile, std::string const &logViewDesc);

#endif /* !_LOG_DESC_HXX_ */
