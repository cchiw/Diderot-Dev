# Makefile for diderot system
#
# This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
#
# COPYRIGHT (c) 2015 The University of Chicago
# All rights reserved.
#

SHELL =		/bin/sh


INSTALL =	/usr/bin/install -c

ifeq (false,true)
BUILD_DIRS =	src/logging/gen
endif

BUILD_DIRS +=	src/compiler \
		src/lib/build

INSTALL_DIR =	/usr/local

# add OpenCL specific targets
#
# FIXME
#
#ifeq (false,true)
#BUILD_DIRS +=	src/clinfo
#endif

.PHONY:		help build local-install install \
		clean local-distclean local-devclean

help:
	@echo "This Makefile supports the following targets:"
	@echo "  help          -- print this message."
	@echo "  build         -- build Diderot tools and libraries"
	@echo "  local-install -- install Diderot tools, libraries, and headers in "
	@echo "                   /Users/chariseechiw/diderot/Diderot-Dev/{bin,lib,include}"
	@echo "  install       -- install Diderot tools, libraries, and headers in "
	@echo "                   @INSTALL_ROOT@/{bin,lib,include}"
	@echo "  clean         -- remove intermediate files generated during building"
	@echo "  distclean     -- remove files generated during configuration"
	@echo "                   and building; the resulting tree has the same"
	@echo "                   files as the distribution."
	@echo "The following additional targets are primarily for developers:"
	@echo "  devclean      -- remove everything that is not part of the SVN"
	@echo "                   repository."

local-install:
	for dir in $(BUILD_DIRS); do \
	  (cd $$dir && $(MAKE) local-install) || exit $$?; \
	done
	mkdir -p include/diderot
	(cd src/lib/include/diderot/; cp -p *.h *.hxx ../../../../include/diderot) || exit $$?

install:
	mkdir -p $(INSTALL_DIR)
	for dir in $(BUILD_DIRS); do \
	  (cd $$dir && $(MAKE) install) || exit $$?; \
	done
	mkdir -p $(INSTALL_DIR)/include/diderot
	(cd src/lib/include/diderot/; cp -p *.h *.hxx $(INSTALL_DIR)/include/diderot) || exit $$?

build:
	for dir in $(BUILD_DIRS); do \
	  (cd $$dir && $(MAKE) build) || exit $$?; \
	done

doc:
	(cd doc; $(MAKE))

#################### Cleanup ####################

CLEAN_SUBDIRS =		$(BUILD_DIRS)

CLEAN_FILES =		rtest/tests/*/out.nrrd

DISTCLEAN_FILES +=	Makefile config.status config.log \
			autom4te*.cache \
			bin \
			lib \
			include \
                        rtest/scripts/run.sh \
                        rtest/scripts/run-one.sh \
			rtest/log.* \
			rtest/report.* \
			src/lib/include/diderot/config.h \
			src/lib/include/diderot/log-events.hxx \
			src/lib/include/diderot/logging.hxx

DEVCLEAN_FILES =	configure \
			config/config_h.in

include /Users/chariseechiw/diderot/Diderot-Dev/mk/clean-rules.gmk
