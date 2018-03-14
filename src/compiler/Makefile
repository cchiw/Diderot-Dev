# Makefile.in
#
# This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
#
# COPYRIGHT (c) 2015 The University of Chicago
# All rights reserved.
#
# src/compiler/Makefile.  Generated from Makefile.in by configure.
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
HEAPDIR =	$(BINDIR)/.heap

INSTALL =	/usr/bin/install -c
SHELL =		/bin/sh


#
# generator tools
#
MKMK =		$(SRCDIR)/compiler/gen/fragments/mkmk.sh
MKFRAGS =	$(SRCDIR)/compiler/gen/fragments/mkfrags.sh

#
# SML/NJ build tools
#
INSTALL_SMLNJ_WRAPPER = /Users/chariseechiw/diderot/femprime/bin/install-sml-wrapper.sh
ML_BUILD =		/usr/local/smlnj/bin/ml-build
ML_BUILD_FLAGS =	-Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm
ML_MAKEDEPEND =		/usr/local/smlnj/bin/ml-makedepend
ML_MAKEDEPEND_FLAGS =

#### End of system configuration section. ####

PROGRAM =		diderotc
HEAP_IMAGE =		$(PROGRAM).x86-darwin
ROOT_CM =		driver/sources.cm
CM_FILES =		ast/sources.cm \
			basis/sources.cm \
			cfg-ir/sources.cm \
			codegen/sources.cm \
			common/sources.cm \
			cxx-util/sources.cm \
			driver/sources.cm \
			ein/sources.cm \
			fields/sources.cm \
			global-env/sources.cm \
			high-ir/sources.cm \
			low-ir/sources.cm \
			mid-ir/sources.cm \
			nrrd/sources.cm \
			options/sources.cm \
			parse-tree/sources.cm \
			parser/sources.cm \
			simple/sources.cm \
			simplify/sources.cm \
			translate/sources.cm \
			typechecker/sources.cm

CODE_FRAG_DIRS =	codegen cxx-util target-cpu

CODE_FRAG_FILES =	$(patsubst %,%/fragments.sml,$(CODE_FRAG_DIRS))
CODE_FRAG_MKFILES =	$(patsubst %,%/fragments.gmk,$(CODE_FRAG_DIRS))

.PHONY:		local-install
local-install:	$(HEAP_IMAGE)
	$(INSTALL_SMLNJ_WRAPPER) $(PROGRAM) $(BINDIR)

.PHONY:		install
install:	$(HEAP_IMAGE)
	$(INSTALL_SMLNJ_WRAPPER) $(PROGRAM) $(INSTALL_BINDIR)

.PHONY:		build
build:		$(HEAP_IMAGE)

$(HEAP_IMAGE):	.depend $(CODE_FRAG_MKFILES)
	$(ML_BUILD) $(ML_BUILD_FLAGS) $(ROOT_CM) Main.main $(PROGRAM)
	touch $(HEAP_IMAGE)

#
# Rules for generating the fragment dependency and source files
#

codegen/fragments.gmk: codegen/fragments/CATALOG $(wildcard codegen/fragments/*in)
	$(MKMK) codegen

codegen/fragments.sml:
	$(MKFRAGS) codegen

cxx-util/fragments.gmk: cxx-util/fragments/CATALOG $(wildcard cxx-util/fragments/*in)
	$(MKMK) cxx-util

cxx-util/fragments.sml:
	$(MKFRAGS) cxx-util

target-cpu/fragments.gmk: target-cpu/fragments/CATALOG $(wildcard target-cpu/fragments/*in)
	$(MKMK) target-cpu

target-cpu/fragments.sml:
	$(MKFRAGS) target-cpu

#
# Dependency file rules
#
.depend:	$(CM_FILES) $(CODE_FRAG_FILES)
	touch .depend
	$(ML_MAKEDEPEND) $(ML_MAKEDEPEND_FLAGS) -n -f .depend $(ROOT_CM) $(HEAP_IMAGE) || rm -f .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),devclean)
ifneq ($(MAKECMDGOALS),distclean)
sinclude .depend
sinclude codegen/fragments.gmk
sinclude cxx-util/fragments.gmk
sinclude target-cpu/fragments.gmk
endif
endif
endif

#################### Cleanup ####################

CLEAN_SUBDIRS =

CLEAN_FILES +=		$(HEAP_IMAGE) \
			*/*.grm.sml \
			*/*.lex.sml \
			$(CODE_FRAG_FILES) \
			$(CODE_FRAG_MKFILES) \
			.depend

DISTCLEAN_FILES +=	Makefile \
			common/paths.sml \
			common/size-of.sml \
			nrrd/run-dnorm.sml \
			options/version.sml \
			$(MKMK) $(MKFRAGS)

DEVCLEAN_FILES =

include /Users/chariseechiw/diderot/femprime/mk/clean-rules.gmk
