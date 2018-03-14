/*! \file load-log.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _LOAD_LOG_HXX_
#define _LOAD_LOG_HXX_

#include <string>
#include <vector>

/* internal representation of event occurrences */
struct event {
    uint64_t		_ts;		// time stamp
    uint16_t		_worker;	// worker ID
    uint16_t		_evt;		// event ID
    uint32_t            _sid;		// optional strand ID

    event (uint64_t ts, uint16_t wid, uint16_t evt, uint32_t sid = 0)
	: _ts(ts), _worker(wid), _evt(evt), _sid(sid)
    { }

};

struct log_file {
    std::string        	_date;      	// the date of the run (as reported by ctime(3))
    std::string        	_clockName; 	// a string describing the clock
    uint64_t            _startTime;     // start time from header
    uint32_t    	_resolution;    // clock resolution in nanoseconds
    uint32_t    	_nNodes;        // number of nodes
    uint32_t    	_nCores;        // number of cores
    uint32_t    	_nWorkers;      // number of worker threads
    std::vector<event>	_events;	// the events sorted in timestamp order
};

// load a log file
//
log_file *load_log_file (std::string const &name, bool noSort);

#endif // !_LOAD_LOG_HXX_
