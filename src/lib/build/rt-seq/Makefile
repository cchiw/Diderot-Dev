# Makefile
#
# This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
#
# COPYRIGHT (c) 2016 The University of Chicago
# All rights reserved.
#
# Generic makefile for the runtime system libraries.  This file is used to
# create the Makefile.in files for rt-c-f, rt-c-f-debug, etc.
#
# src/lib/build/rt-seq/Makefile.  Generated from Makefile.in by configure.
#

#### Start of system configuration section. ####

#
# directories for the install target
#
PREFIX =		/usr/local
INSTALL_BINDIR =	$(PREFIX)/bin
INSTALL_HEAPDIR =	$(INSTALL_BINDIR)/.heap
INSTALL_LIBDIR =	$(PREFIX)/lib
INSTALL_INCDIR =	$(PREFIX)/include

#
# directories for the local-install target
#
SRCDIR =	/Users/chariseechiw/diderot/femprime/src
LIBDIR =	/Users/chariseechiw/diderot/femprime/lib
BINDIR =	/Users/chariseechiw/diderot/femprime/bin

INSTALL =	/usr/bin/install -c
SHELL =		/bin/sh


#### End of system configuration section. ####

# the target platform
#
TARGET_PLATFORM = sequential

# the target source directory
#
TARGET_SRCDIR = $(SRCDIR)/lib/sequential-target

# the target build options
#
BUILD_OPTIONS =	

# target-platform specific sources
#
TARGET_SRCS =	$(notdir $(wildcard $(TARGET_SRCDIR)/*.cxx))

#
# View path
#
VPATH =		$(SRCDIR)/lib/include:$(SRCDIR)/lib/common:$(TARGET_SRCDIR)


# the target library name
#
TARGET =	diderot-rt-seq.o

.PHONY:		build local-install install
build:		$(TARGET)

local-install:	$(TARGET)
	$(INSTALL) $(TARGET) $(LIBDIR)/$(TARGET)

install:	$(TARGET)
	$(INSTALL) $(TARGET) $(INSTALL_LIBDIR)/$(TARGET)

# Include common definitions for run-time builds
#
include ../mk/build.gmk
