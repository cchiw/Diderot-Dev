/*! \file load-log.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include "diderot/log-file.hxx"
#include "load-log.hxx"
#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <sys/stat.h>

// comparison fucntion for events.  Returns true if ev1 < ev2 when ordered
// first by time stamps and then by worker ID.
//
static bool compare_events (event const &ev1, event const &ev2)
{
    return (ev1._ts < ev2._ts) || ((ev1._ts == ev2._ts) && (ev1._worker < ev2._worker));
}

// convert a timestamp to nanoseconds
//
inline uint64_t get_timestamp (int kind, const diderot::log::time_stamp *ts)
{
    if (kind == diderot::log::TS_MACH_ABSOLUTE)
	return ts->_mach;
    else if (kind == diderot::log::TS_TIMESPEC)
	return static_cast<int64_t>(ts->_tv._sec) * 1000000000 + static_cast<int64_t>(ts->_tv._frac);
    else /* kind == diderot::log::TS_TIMEVAL */
	return static_cast<int64_t>(ts->_tv._sec) * 1000000000 + static_cast<int64_t>(ts->_tv._frac) * 1000;
}

static void dump_buffer (diderot::log::file_hdr const &hdr, diderot::log::buffer const &buf)
{
    auto wid = std::cerr.width();
    std::cerr << "##### BUFFER: worker = " << buf._worker << "; seq# = " << buf._seqNum << "\n";
    for (uint32_t ix = 0;  ix < buf._next;  ix++) {
        const diderot::log::event *ep = &buf._log[ix];
	std::cerr << std::setw(3) << ix << ":";
	const uint32_t *ip = reinterpret_cast<const uint32_t *>(ep);
	std::cerr << std::setfill('0') << std::hex;
	for (uint32_t j = 0;  j < sizeof(event)/4;  j++) {
	    std::cerr << " " << std::setw(8) << ip[j];
	}
	std::cerr << std::setfill(' ') << std::dec << "  { ";
        uint64_t ts = get_timestamp(hdr._tsKind, &ep->_ts);
        std::cerr << "[" << (ts / 1000000000) << "."
	    << std::setfill('0') << std::setw(9) << (ts % 1000000000)
	    << std::setfill(' ') << std::setw(wid)
	    << "] " << ep->_strand << " " << ep->_event << "}\n";

    }
    std::cerr << "#####\n";
}

// load a log file
//
log_file *load_log_file (std::string const &name, bool noSort)
{
  // get the file size and compute the expected number
  // of buffers.
    int numBufs;
    {
	struct stat st;
	if (stat(name.c_str(), &st) < 0) {
	    perror ("stat");
	    return nullptr;
	}
	numBufs = (st.st_size - sizeof(diderot::log::file_hdr)) / sizeof(diderot::log::buffer);
	if (st.st_size != sizeof(diderot::log::file_hdr) + numBufs * sizeof(diderot::log::buffer)) {
	    std::cerr << "Warning: log file size is not a whole number of buffers\n";
	}
    }

  // open the file
    std::ifstream ins(name, std::ifstream::in | std::ifstream::binary);
    if (ins.fail()) {
	std::cerr << "Error: unable to open \"" << name << "\"\n";
	return nullptr;
    }

  // read the header
    diderot::log::file_hdr hdr;
    ins.read (reinterpret_cast<char *>(&hdr), sizeof(hdr));
    if (ins.fail()) {
	std::cerr << "Error attempting to read header from \"" << name << "\"\n";
	return nullptr;
    }

  // check the header
    if (hdr._magic != diderot::log::file_hdr::MAGIC) {
	std::cerr << "bogus magic number\n";
	return nullptr;
    }
    if (hdr._version[0] != DIDEROT_LOG_VERSION_MAJOR) {
	std::cerr << "wrong version = " << hdr._version[0] << "." << hdr._version[1]
	    << "." << hdr._version[2] << " (expected " << DIDEROT_LOG_VERSION_MAJOR << ".x.y)\n";
	return nullptr;
    }
    if (hdr._version[1] != 0) {
	std::cerr << "fat-event format not supported\n";
	exit (1);
    }
    if (hdr._hdrSzB != sizeof(diderot::log::file_hdr)) {
	std::cerr << "bogus header size " << hdr._hdrSzB << " (expected "
	    << sizeof(diderot::log::file_hdr) << ")\n";
	return nullptr;
    }
    if (hdr._eventSzB != sizeof(diderot::log::event)) {
	std::cerr << "using different event size " << hdr._eventSzB << " (expected "
	    << sizeof(diderot::log::event) << ")\n";
	return nullptr;
    }
    if (hdr._bufSzB != diderot::log::buffer::SIZEB) {
	std::cerr << "using different block size " << hdr._bufSzB << " (expected "
	    << diderot::log::buffer::SIZEB << ")\n";
	return nullptr;
    }

  // get the start time
    uint64_t startTime = get_timestamp(hdr._tsKind, &hdr._startTime);

  // compute an upper bound on the number of events in the file
    uint64_t maxNumEvents = diderot::log::buffer::NUM_EVENTS * numBufs;

  // initialize the in-memory log file
    log_file *log = new log_file;
    log->_date = std::string(hdr._date);
    log->_clockName = std::string(hdr._clockName);
    log->_startTime = startTime;
    log->_resolution = hdr._resolution;
    log->_nNodes = hdr._nNodes;
    log->_nCores = hdr._nCores;
    log->_nWorkers = hdr._nWorkers;
    log->_events.reserve (maxNumEvents);

  // track last timestamp per worker to detect anomolies
    uint64_t lastTS[hdr._nWorkers];
    for (int i = 0;  i <= hdr._nWorkers;  i++) {
	lastTS[i] = startTime;
    }

  // read in the events
    diderot::log::buffer buf;
    uint64_t numEvents = 0;
    for (int i = 0;  i < numBufs;  i++) {
	ins.read (reinterpret_cast<char *>(&buf), sizeof(buf));
	if (ins.fail()) {
	    std::cerr << "Error attempting to read buffer from \"" << name << "\"\n";
	    return nullptr;
	}
	if (buf._next > diderot::log::buffer::NUM_EVENTS)
	    buf._next = diderot::log::buffer::NUM_EVENTS;
	uint16_t worker = buf._worker;
	if (log->_nWorkers < worker) {
	    std::cerr << "invalid worker ID " << worker << " for buffer (expected 0.."
		<< log->_nWorkers << ")\n";
	    return nullptr;
	}
	for (int j = 0;  j < buf._next;  j++, numEvents++) {
	    diderot::log::event *ep = &(buf._log[j]);
	    uint64_t ts = get_timestamp(hdr._tsKind, &(ep->_ts));
	    if (ts < lastTS[worker]) {
		std::cerr << "warning: timestamp for worker " << worker
		    << "; event #" << buf._seqNum << "." << j << "(=" << ep->_event << ") occurs "
		    << lastTS[worker]-ts << "ns too early\n";
                dump_buffer (hdr, buf);
	    }
	    lastTS[worker] = ts;
	    log->_events.push_back (event (ts, worker, ep->_event, ep->_strand));
	}
    }
    ins.close();

    if (noSort || (numEvents == 0)) {
	return log;
    }

  // sort the events by <timestamp, worker-id>
    std::sort (log->_events.begin(), log->_events.end(), compare_events);

  // Adjust the timestamps to be relative to the start of the run
    if (log->_events[0]._ts < startTime) {
	std::cerr << "Warning: first event occurs " << (startTime - log->_events[0]._ts)
	    << " ns. before start time\n";
	startTime = log->_events[0]._ts;
    }
    for (auto it = log->_events.begin(); it != log->_events.end();  ++it) {
	it->_ts -= startTime;
    }

    return log;

}
