#
# project  LilyPond -- the musical typesetter
# title	   generic make rules
# file	   make/Rules.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# this is supposed to clear all suffixes:
.SUFFIXES:
# so why does make still consider xx.y : RCS/xx.y,v ?
# there is no suffix ,v anymore!
.SUFFIXES: .cc .o .hh .y .l .pod .txt .1 .dep

# cancel implicit rules:
#
# shit, how to get rid of these stupid built-in rules?
# include ./$(depth)/make/Builtin-rules.make
#

# compile rules:
#
$(outdir)/%.o: %.cc
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(CXX_OUTPUT_OPTION) 

$(outdir)/%.cc: %.y
#	$(BISON) -d $<
	$(BISON) $<
#	mv $(shell basename $@ .cc ).tab.h $(include-lib)/$(shell basename $@ .cc).hh
#	mv $(shell basename $@ .cc ).tab.h $(outdir)/$(shell basename $@ .cc).hh
	mv $(shell basename $@ .cc ).tab.c $@

$(outdir)/%.hh: %.y
	$(BISON) -d $<
	mv $(shell basename $@ .hh ).tab.h $@
	mv $(shell basename $@ .hh ).tab.c $(outdir)/$(shell basename $@ .hh).cc

$(outdir)/%.cc: %.l
	$(FLEX)  -t $< > $@

$(outdir)/%.text: $(outdir)/%.1
	groff -man -Tascii $< > $@

$(depth)/%.text: $(outdir)/%.text
	cp $< $@

$(outdir)/%.1: %.pod
	pod2man --center="LilyPond documentation" --section="0"\
		--release="LilyPond $(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL)" $< > $@
#

# outdirs:
#
# ?$(outdir)/%.dep:
%.dep:
	touch $@


# build and config stuff: (could make this generic default rule...)
#
%/.build:
	echo 0 >$@


# specific stuff:
#
$(LIBFLOWER): check-flower-deps
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/flower/lib

check-flower-deps:
	$(MAKE)  -C $(depth)/flower/lib

check-lily-deps: check-flower-deps
	$(MAKE)  -C $(depth)/lib

$(LIBLILY): dummy
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/lib
#

