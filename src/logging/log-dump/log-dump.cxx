/*! \file log-dump.cxx
 *
 * \author John Reppy
 *
 * A simple program for dummping out a sorted history from a log file.
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
#include <cstdio>
#include <cstring>

/* TODO: make these variables that are settable from command-line */
#define logDescFile	DEFAULT_LOG_EVENTS_PATH
#define logViewFile	DEFAULT_LOG_VIEW_PATH

static void print_event (LogFileDesc *lfd, FILE *out, event const &evt)
{

    fprintf (out, "[%3d.%09d] ", (int)(evt._ts / 1000000000), (int)(evt._ts % 1000000000));
    for (int i = 0;  i < evt._worker;  i++) {
	fprintf(out, " %20s", " ");
    }
    std::string tag = lfd->FindEventById(evt._evt)->Name();
    char buf[21];
    int n = tag.length();
    strncpy(buf, tag.c_str(), (n > 20) ? 20 : n);
    buf[(n > 20) ? 20 : n] = '\0';
    fprintf (out, "%-20s\n", buf);

}

static void usage (int sts)
{
    fprintf (stderr, "usage: log-dump [-o outfile] [-log logfile] [-nosort]\n");
    exit (sts);
}

int main (int argc, const char **argv)
{
    const char *logFile = nullptr;
    FILE *out = stdout;
    bool noSort = false;

  // process args
    for (int i = 1;  i < argc; ) {
	if (strcmp(argv[i], "-h") == 0) {
	    usage (0);
	}
	else if (strcmp(argv[i], "-o") == 0) {
	    if (++i < argc) {
		out = fopen(argv[i], "w"); i++;
		if (out == NULL) {
		    perror("fopen");
		    exit(1);
		}
	    }
	    else {
		fprintf(stderr, "missing filename for \"-o\" option\n");
		usage (1);
	    }
	}
	else if (strcmp(argv[i], "-log") == 0) {
	    if (++i < argc) {
		logFile = argv[i]; i++;
	    }
	    else {
		fprintf(stderr, "missing filename for \"-log\" option\n");
		usage (1);
	    }
	}
	else if (strcmp(argv[i], "-nosort") == 0) {
	    noSort = true;
	    i++;
	}
	else {
	    fprintf(stderr, "invalid argument \"%s\"\n", argv[i]);
	    usage(1);
	}
    }
    if (logFile == nullptr) {
	usage(1);
    }

    LogFileDesc *logFileDesc = LoadLogDesc (logDescFile, logViewFile);
    if (logFileDesc == nullptr) {
	fprintf(stderr, "unable to load \"%s\"\n", logDescFile);
	exit (1);
    }

    log_file *log = load_log_file (logFile, noSort);

    fprintf (out, "Log taken on %s\n", log->_date.c_str());
    fprintf (out, "%d workers, %d cores; %d events; clock = %s (%dns resolution)\n",
	log->_nWorkers, log->_nCores, (int)log->_events.size(), log->_clockName.c_str(),
        log->_resolution);

    if (noSort) {
        fprintf (out, "start time = %3d.%09d\n",
	    (int)(log->_startTime / 1000000000), (int)(log->_startTime % 1000000000));
    }

    for (auto it = log->_events.begin(); it != log->_events.end();  ++it) {
	print_event (logFileDesc, out, *it);
    }

}
