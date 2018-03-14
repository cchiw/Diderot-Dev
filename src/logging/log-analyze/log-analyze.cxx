/*! \file log-analyze.cxx
 *
 * \author John Reppy
 *
 * A program for analyzing the scheduling behavior of a Diderot application from
 * a log file.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include "load-log.hxx"
#include "event-desc.hxx"
#include "log-desc.hxx"
#include "default-log-paths.hxx"
#include "quantize.hxx"
#include "svg.hxx"
#include <iostream>
#include <iomanip>
#include <cstring>

#define MICROSEC	1000
#define MILLISEC	(1000 * MICROSEC)
#define TIMEQ           (50 * MILLISEC)		// 50ms buckets

/* TODO: make these variables that are settable from command-line */
#define logDescFile	DEFAULT_LOG_EVENTS_PATH
#define logViewFile	DEFAULT_LOG_VIEW_PATH

// worker states
enum {
    Dead = 0,		// before start and after exit
    Waiting,            // blocked on gate or barrier
    Running,            // executing strands
    Other,              // other running time
    NStates
};

void genSVG (std::ostream &outS, class buckets const &Q);

// convert nanoseconds to seconds
inline double toSeconds (uint64_t ns)
{
    return static_cast<double>(ns / 1000000000) + static_cast<double>(ns % 1000000000) * 10e-9;
}

struct worker_stats {
    uint32_t            _id;		// worker ID (0 == controller)
    uint32_t            _state;		// current state
    uint64_t		_start;		// time of worker start
    uint64_t            _end;		// time of worker exit
    uint64_t		_last;		// time of last state change
    uint64_t		_time[NStates];
    uint32_t		_nBlocks;
    uint32_t		_nStarts;
    uint32_t		_nUpdates;
    uint32_t		_nStabilizes;
    uint32_t		_nDies;

    worker_stats (uint32_t id)
	: _id(id), _state(Dead), _last(0), _time{0, 0, 0, 0},
	  _nBlocks(0), _nStarts(0), _nUpdates(0), _nStabilizes(0), _nDies(0)
    { }

    void set_state (uint32_t state, uint64_t ts);

    double run_time () const { return toSeconds(this->_end - this->_start); }
    double time_waiting () const { return toSeconds(this->_time[Waiting]); }
    double time_running () const { return toSeconds(this->_time[Running]); }
    double time_overhead () const { return toSeconds(this->_time[Other]); }
    double pct_waiting () const
    {
	return (100.0 * this->time_waiting()) / this->run_time();
    }
    double pct_running () const
    {
	return (100.0 * this->time_running()) / this->run_time();
    }
    double pct_overhead () const
    {
	return (100.0 * this->time_overhead()) / this->run_time();
    }

};

// per-time quantum information
struct info {
    uint32_t	totTime;		// total time (in ns)
    uint32_t	time[NStates];		// time per state (in ns)
    uint32_t	nStarts;		// number of StrandStartEvt events
    uint32_t	nUpdates;
    uint32_t	nStabilizes;
    uint32_t	nDies;

    info() { bzero (this, sizeof(info)); }
    ~info () { }

    void addTime (uint32_t state, uint64_t t)
    {
	time[state] += (uint32_t)t;
    }

  // return percent (between 0 and 1) of time spent in state sid.
    const double pct (uint32_t sid) const
    {
	uint64_t t = this->time[sid];
	if (t == 0) {
	    return 0.0;
	}
	double w = static_cast<double>(t);
        w /= static_cast<double>(this->totTime);
        return w;
    }

};

class buckets : public intervals<info> {
  public:

    buckets (uint32_t nThreads, uint32_t nIntervals, uint64_t timeQ)
      : intervals<info> (nThreads, nIntervals, timeQ),
	_lastT(new uint64_t[nThreads]),
	_state(new uint32_t[nThreads])
    {
      // initialize arrays
	for (uint32_t i = 0;  i < nThreads;  i++) {
	    this->_lastT[i] = 0;
	    this->_state[i] = 0;
	}
    }

    virtual ~buckets ()
    {
	delete[] _lastT;
	delete[] _state;
    }

    uint64_t time (uint32_t tid, uint32_t bid) const
    {
	return this->_info(tid, bid).totTime;
    }

  // return percent (between 0 and 1) of time spent in state sid for
  // worker tid and bucket bid.
    const double pct (uint32_t tid, uint32_t bid, uint32_t sid) const
    {
	return this->_info(tid, bid).pct(sid);
    }

    void setState (uint32_t tid, uint32_t sid, uint64_t t)
    {
	this->_addTime (tid, this->_state[tid], this->_lastT[tid], t);
	this->_state[tid] = sid;
	this->_lastT[tid] = t;
    }

    void finish (uint64_t endT);

  protected:
    uint64_t	*_lastT;	// last event time per worker
    uint32_t	*_state;	// current state per worker

    void _addTime (uint32_t tid, uint32_t sid, uint64_t startT, uint64_t endT);

};

void buckets::_addTime (uint32_t tid, uint32_t sid, uint64_t startT, uint64_t endT)
{
    if (endT <= startT) {
	return;
    }
    if (this->_runT < endT) {
	std::cerr << "addTime: end time " << endT << " is after run time " << this->_runT << "\n";
	exit (1);
    }
    uint32_t startB = startT / this->_timeQ;
    uint32_t endB = endT / this->_timeQ;
    if (startB == endB) {
	this->_info(tid, startB).addTime(sid, endT - startT);
    }
    else {
	uint64_t t = (startB + 1) * this->_timeQ;
	while (t < endT) {
	    this->_info(tid, startB).addTime(sid, t - startT);
	    startT = t;
	    t += this->_timeQ;
	    startB++;
	}
	this->_info(tid, endB).addTime(sid, endT - startT);
    }
}

void buckets::finish (uint64_t endT)
{
  // add total time per bucket
    for (uint32_t i = 0;  i < this->_nThreads;  i++) {
	this->_addTime (i, Dead, this->_lastT[i], endT);
	for (uint32_t j = 0;  j < this->_nIntervals;  j++) {
	    uint64_t t = 0;
	    uint32_t idx = this->_index(i, j);
	    for (uint32_t k = 0;  k < NStates;  k++) {
		t += this->_interval[idx].time[k];
	    }
	    this->_interval[idx].totTime = t;
	}
    }
}

void report (std::ostream &outS, log_file *, std::vector<worker_stats> const &stats);

static void usage (int sts)
{
    fprintf (stderr, "usage: log-analyze [-o outfile] [-log logfile] [-svg]\n");
    exit (sts);
}

int main (int argc, const char **argv)
{
    std::string logFile("");
    std::string outFile("-");
    bool outputSVG = false;
    uint64_t timeQ = TIMEQ;

  // process args
    for (int i = 1;  i < argc; ) {
	if (strcmp(argv[i], "-h") == 0) {
	    usage (0);
	}
	else if (strcmp(argv[i], "-o") == 0) {
	    if (++i < argc) {
		outFile = argv[i];
		i++;
	    }
	    else {
		std::cerr << "missing filename for \"-o\" option\n";
		usage (1);
	    }
	}
	else if (strcmp(argv[i], "-log") == 0) {
	    if (++i < argc) {
		logFile = argv[i];
		i++;
	    }
	    else {
		std::cerr << "missing filename for \"-log\" option\n";
		usage (1);
	    }
	}
	else if (strcmp(argv[i], "-svg") == 0) {
	    i++;
	    outputSVG = true;
	}
	else if (strcmp(argv[i], "-tq") == 0) {
	    if (++i < argc) {
		timeQ = std::stoull(argv[i]) * MILLISEC;
		i++;
	    }
	    else {
		std::cerr << "missing value for \"-tq\" option\n";
		usage (1);
	    }
	}
	else {
	    std::cerr << "invalid argument \"" << argv[i] << "\"\n";
	    usage(1);
	}
    }
    if (logFile.length() == 0) {
	usage(1);
    }

    LogFileDesc *logFileDesc = LoadLogDesc (logDescFile, logViewFile);
    if (logFileDesc == nullptr) {
	std::cerr << "unable to load \"" << logDescFile << "\"\n";
	exit (1);
    }

    log_file *log = load_log_file (logFile, false);
    if (log == nullptr) {
	std::cerr << "unable to load \"" << logFile << "\"\n";
	exit (1);
    }
    uint64_t maxTime = log->_events.back()._ts;

    std::vector<worker_stats> stats;
    stats.reserve(log->_nWorkers+1);
    for (uint32_t i = 0;  i <= log->_nWorkers;  i++) {
	stats.push_back(worker_stats(i));
    }

    buckets Q(log->_nWorkers+1, (maxTime + timeQ - 1) / timeQ, timeQ);

  // process the event log
    for (auto it = log->_events.begin(); it != log->_events.end();  ++it) {
	int id = it->_worker;
	switch (it->_evt) {
	  case diderot::log::SchedulerStartEvt:
	    stats[id]._start = it->_ts;
	    stats[id].set_state (Other, it->_ts);
	    Q.setState (id, Other, it->_ts);
	    break;
	  case diderot::log::SchedulerShutdownEvt:
	    stats[id]._end = it->_ts;
	    stats[id].set_state (Dead, it->_ts);
	    Q.setState (id, Dead, it->_ts);
	    break;
	  case diderot::log::WorkerStartEvt:
	    stats[id]._start = it->_ts;
	    stats[id].set_state (Other, it->_ts);
	    Q.setState (id, Other, it->_ts);
	    break;
	  case diderot::log::WorkerExitEvt:
	    stats[id]._end = it->_ts;
	    stats[id].set_state (Dead, it->_ts);
	    Q.setState (id, Dead, it->_ts);
	    break;
	  case diderot::log::GetStrandBlockEvt:
	    stats[id]._nBlocks++;
	    stats[id].set_state (Waiting, it->_ts);
	    Q.setState (id, Waiting, it->_ts);
	    break;
	  case diderot::log::GotStrandBlockEvt:
	    stats[id]._nBlocks++;
	    stats[id].set_state (Running, it->_ts);
	    Q.setState (id, Running, it->_ts);
	    break;
	  case diderot::log::NoStrandBlockEvt:
	    stats[id].set_state (Other, it->_ts);
	    Q.setState (id, Other, it->_ts);
	    break;
	  case diderot::log::WorkerGateWaitEvt:
	    stats[id].set_state (Waiting, it->_ts);
	    Q.setState (id, Waiting, it->_ts);
	    break;
	  case diderot::log::ControllerGateWaitEvt:
	    stats[id].set_state (Waiting, it->_ts);
	    Q.setState (id, Waiting, it->_ts);
	    break;
	  case diderot::log::GateReleaseWorkersEvt:
	    break;
	  case diderot::log::WorkerGateResumeEvt:
	    stats[id].set_state (Other, it->_ts);
	    Q.setState (id, Other, it->_ts);
	    break;
	  case diderot::log::ControllerGateResumeEvt:
	    stats[id].set_state (Other, it->_ts);
	    Q.setState (id, Other, it->_ts);
	    break;
	  case diderot::log::BarrierWaitEvt:
	    stats[id].set_state (Waiting, it->_ts);
	    Q.setState (id, Waiting, it->_ts);
	    break;
	  case diderot::log::BarrierResumeEvt:
	    stats[id].set_state (Other, it->_ts);
	    Q.setState (id, Other, it->_ts);
	    break;
	  case diderot::log::StrandStartEvt:
	    stats[id]._nStarts++;
	    stats[id].set_state (Running, it->_ts);
	    Q.setState (id, Running, it->_ts);
	    break;
	  case diderot::log::StrandUpdateEvt:
	    stats[id]._nUpdates++;
	    stats[id].set_state (Running, it->_ts);
	    Q.setState (id, Running, it->_ts);
	    break;
	  case diderot::log::StrandDieEvt:
	    stats[id]._nDies++;
	    stats[id].set_state (Running, it->_ts);
	    Q.setState (id, Running, it->_ts);
	    break;
	  case diderot::log::StrandStabilizeEvt:
	    stats[id]._nStabilizes++;
	    stats[id].set_state (Running, it->_ts);
	    Q.setState (id, Running, it->_ts);
	    break;
	  default:
	    break;
	}
    }

    Q.finish(maxTime);

    std::ostream *outS = nullptr;
    bool needsClose = false;

    if (outFile.compare("-") == 0) {
	outS = &std::cout;
    }
    else {
	outS = new std::ofstream(outFile);
	if (! outS->good()) {
	    std::cerr << "unable to open output file \"" << outFile << "\"\n";
	    exit (1);
	}
	needsClose = true;
    }

    if (outputSVG) {
	svg::output_header (*outS);
	(*outS) << "<!--\n";
	report (*outS, log, stats);
	(*outS) << "-->\n";
	genSVG (*outS, Q);
    }
    else {
	report (*outS, log, stats);
    }

    if (needsClose) {
	delete outS;
    }

    return 0;

}

struct layout {
    int32_t	_topMargin;
    int32_t	_leftMargin;
    int32_t	_hSep;
    int32_t	_wid;

    svg::Point position (int32_t thd, int32_t bucket)
    {
	double x = this->_leftMargin + (this->_hSep + this->_wid) * thd;
	double y = this->_topMargin + this->_wid * bucket;
	return svg::Point(x, y);
    }

    svg::Dimensions dimensions (int32_t nThreads, int32_t nBuckets)
    {
	double wid = this->_leftMargin + (this->_hSep * (nThreads-1)) + this->_wid * nThreads + 2;
	double ht = this->_topMargin + this->_wid * nBuckets + 2;
	return svg::Dimensions (wid, ht);
    }

};

static layout Layout = {50, 50, 10, 20};

void genSVG (std::ostream &outS, buckets const &Q)
{
    svg::Document doc (Layout.dimensions(Q.numThreads(), Q.numIntervals()));

    for (int32_t tid = 0;  tid < Q.numThreads();  tid++) {
	svg::Group tgrp;
	for (int32_t bid = 0;  bid < Q.numIntervals();  bid++) {
	    svg::Point pt = Layout.position (tid, bid);
	    svg::Group bgrp;
	    bgrp << svg::Stroke(svg::Color("black"));
	    if (Q.time(tid, bid) == 0) {
		bgrp << svg::Fill(svg::Color(0,0,0)) << svg::FillOpacity(0.1);
	    }
	    else {
		double runFrac = Q.pct (tid, bid, Running);
		double waitFrac = Q.pct (tid, bid, Waiting);
		double otherFrac = Q.pct (tid, bid, Other);
		double opacity;
		if ((runFrac > waitFrac) && (runFrac > otherFrac)) {
		    bgrp << svg::Fill(svg::Color(0,128,0));
		    opacity = 0.3 + 0.7 * runFrac;
		}
		else if ((waitFrac >= runFrac) && (waitFrac >= otherFrac)) {
		    bgrp << svg::Fill(svg::Color(255,0,0));
		    opacity = 0.3 + 0.7 * waitFrac;
		}
		else {
		    bgrp << svg::Fill(svg::Color(0,0,255));
		    opacity = 0.3 + 0.7 * otherFrac;
		}
		bgrp << svg::FillOpacity(opacity);
	    }
	    bgrp << svg::Path{svg::path::M(pt),
		    svg::path::l(Layout._wid,0),
		    svg::path::l(0,Layout._wid),
		    svg::path::l(-Layout._wid,0),
		    svg::path::Z()
		};
	    tgrp << bgrp;
	}
	doc << tgrp;
    }

    doc.serialize (outS);
}

void report (std::ostream &outS, log_file *log, std::vector<worker_stats> const &stats)
{
    outS << "Log taken on " << log->_date << "\n";
    outS << log->_nWorkers << " workers, "
	<< log->_nCores << " cores, " << log->_events.size() << " events, clock = "
	<< log->_clockName << " (" << log->_resolution << "ns resolution)\n";

    outS << std::fixed;

  // report on the controller

  // check for any StrandDie events
    bool anyDies = false;
    for (uint32_t i = 1;  i <= log->_nWorkers;  i++) {
	if (stats[i]._nDies > 0) {
	    anyDies = true;
	    break;
	}
    }

  // report on the workers
    uint64_t totRunTime = 0;
    uint64_t nStabilizes = 0;
    for (uint32_t i = 0;  i <= log->_nWorkers;  i++) {
	if (i == 0) {
	    outS << "Controller:\n";
	}
	else {
	    totRunTime += stats[i]._time[Running];
	    nStabilizes += stats[i]._nStabilizes;
	    outS << "[" << std::setw(2) << i << "] " << stats[i]._nBlocks << " blocks";
	    if (stats[i]._nBlocks > 0) {
		outS << "; " << stats[i]._nStarts << " starts; "
		    << stats[i]._nUpdates << " updates; "
		    << stats[i]._nStabilizes << " stabilizes";
		if (anyDies) {
		    outS << "; " << stats[i]._nDies << " dies\n";
		}
		else {
		    outS << "\n";
		}
	    }
	    else {
		outS << "\n";
	    }
	}
        outS << "     time: total =    "
	    << std::setw(6) << std::setprecision(3) << stats[i].run_time() << "\n";
        outS << "           waiting =  "
	    << std::setw(6) << std::setprecision(3) << stats[i].time_waiting()
	    << " (" << std::setw(4) << std::setprecision(1) << stats[i].pct_waiting() << "%)\n";
        outS << "           running =  "
	    << std::setw(6) << std::setprecision(3) << stats[i].time_running()
	    << " (" << std::setw(4) << std::setprecision(1) << stats[i].pct_running() << "%)\n";
        outS << "           overhead = "
	    << std::setw(6) << std::setprecision(3) << stats[i].time_overhead()
	    << " (" << std::setw(4) << std::setprecision(1) << stats[i].pct_overhead() << "%)\n";
    }

    outS << "Total worker runtime = "
	<< std::setw(6) << std::setprecision(3) << toSeconds(totRunTime)
	<< "; " << nStabilizes << " strands stabilized; throughput = "
	<< (nStabilizes * 1000000000) / totRunTime << " strands/sec\n";
}

void worker_stats::set_state (uint32_t state, uint64_t ts)
{
    if (this->_state == state) {
	return;
    }
    if (ts < this->_last) {
	this->_state = state;
	return;
    }
    this->_time[this->_state] += ts - this->_last;
    this->_state = state;
    this->_last = ts;
}
