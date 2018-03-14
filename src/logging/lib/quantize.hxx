/*! \file quantize.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _QUANTIZE_HXX_
#define _QUANTIZE_HXX_

#include <cstdint>
#include <cassert>

//! base class for representing per-time-interval performance information
//! on a per-thread basis.
//!
template <typename INFO>
class intervals {
  public:

    intervals (uint32_t nThreads, uint32_t nIntervals, uint64_t timeQ);
    virtual ~intervals ();

    int32_t numThreads () const { return this->_nThreads; }
    int32_t numIntervals () const { return this->_nIntervals; }

  protected:
    uint32_t	_nThreads;
    uint32_t	_nIntervals;
    uint64_t	_runT;		// total runtime covered by intervals in ns
    uint64_t	_timeQ;		// time per interval in ns
    INFO	*_interval;	// intervals indexed by (worker and time)

    uint32_t _index (uint32_t tid, uint32_t qid) const
    {
	assert (tid < this->_nThreads);
	assert (qid < this->_nIntervals);
	return qid + this->_nIntervals * tid;
    }

    INFO &_info (uint32_t tid, uint32_t qid) const
    {
	return this->_interval[this->_index(tid, qid)];
    }
};


// for template instantiation

template <typename INFO>
intervals<INFO>::intervals (uint32_t nThreads, uint32_t nIntervals, uint64_t timeQ)
  : _runT(nIntervals*timeQ),
    _timeQ(timeQ),
    _interval(new INFO[nThreads*nIntervals]),
    _nThreads(nThreads),
    _nIntervals(nIntervals)
{
  // initialize arrays
    for (uint32_t i = 0;  i < nThreads;  i++) {
	for (uint32_t j = 0;  j < nIntervals;  j++) {
	    this->_interval[this->_index(i, j)] = INFO();
	}
    }
}

template <typename INFO>
intervals<INFO>::~intervals ()
{
    delete[] this->_interval;
}

#endif //! _QUANTIZE_HXX_
