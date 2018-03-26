# Makefile
#
# This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
#
# COPYRIGHT (c) 2016 The University of Chicago
# All rights reserved.
#
# The Makefile for building all versions of the Diderot runtime system
#
# src/lib/build/Makefile.  Generated from Makefile.in by configure.
#

SHELL =			/bin/sh
INSTALL =		/usr/bin/install -c


# clear out unused file suffixes for implicit rules
#
.SUFFIXES:
.SUFFIXES:	.cxx .hxx

#
# directories for the install target
#
PREFIX =		/usr/local
INSTALL_LIBDIR =	$(PREFIX)/lib

#
# directories for the local-install target
#
LIBDIR =	/Users/chariseechiw/diderot/Diderot-Dev/lib

#
# optional logging support
#
ifeq (false,true)
LOGGING_HXX =		/Users/chariseechiw/diderot/Diderot-Dev/src/lib/include/diderot/logging.hxx
endif

.PHONY:			build local-install install

build:			$(LOGGING_HXX)
	for dir in $(wildcard rt-*) ; do \
	  (cd $$dir; $(MAKE) build) || exit $$?; \
	done

local-install:		$(LOGGING_HXX)
	$(INSTALL) -d $(LIBDIR)
	for dir in $(wildcard rt-*); do \
	  (cd $$dir && $(MAKE) local-install) || exit $$?; \
	done

install:		$(LOGGING_HXX)
	$(INSTALL) -d $(INSTALL_LIBDIR)
	for dir in $(wildcard rt-*); do \
	  (cd $$dir && $(MAKE) install) || exit $$?; \
	done

#
# optional logging support
#
$(LOGGING_HXX):		/Users/chariseechiw/diderot/Diderot-Dev/bin/log-gen
	/Users/chariseechiw/diderot/Diderot-Dev/bin/log-gen

/Users/chariseechiw/diderot/Diderot-Dev/bin/log-gen:
	(cd /Users/chariseechiw/diderot/Diderot-Dev/src/logging/gen; $(MAKE) local-install)

#################### Cleanup ####################

CLEAN_SUBDIRS =		$(wildcard rt-*)
DISTCLEAN_FILES =	$(wildcard rt-*) mk/build.gmk
DEVCLEAN_FILES =

include /Users/chariseechiw/diderot/Diderot-Dev/mk/clean-rules.gmk
