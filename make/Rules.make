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
$(outdir)/%.o: %.cc $(genout)
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(CXX_OUTPUT_OPTION) 

$(outdir)/%.cc: %.y $(genout)
#	$(BISON) -d $<
	$(BISON) $<
#	mv $(shell basename $@ .cc ).tab.h $(include-lib)/$(shell basename $@ .cc).hh
#	mv $(shell basename $@ .cc ).tab.h $(outdir)/$(shell basename $@ .cc).hh
	mv $(shell basename $@ .cc ).tab.c $@

$(outdir)/%.hh: %.y $(genout)
	$(BISON) -d $<
	mv $(shell basename $@ .hh ).tab.h $@
	mv $(shell basename $@ .hh ).tab.c $(outdir)/$(shell basename $@ .hh).cc

$(outdir)/%.cc: %.l $(genout)
	$(FLEX)  -t $< > $@

$(outdir)/%.text: $(outdir)/%.1
	groff -man -Tascii $< > $@

$(outdir)/%.1: %.pod $(genout)
	pod2man --center="LilyPond documentation" --section="0"\
		--release="LilyPond $(MAJVER).$(MINVER).$(PATCHLEVEL)" $< > $@
#

# outdirs:
#
# ?$(outdir)/%.dep:
%.dep:
	touch $@
$(outdir):
	mkdir $(outdir)
	@touch $(genout)
%/$(outdir):y
	mkdir $@
	@touch $(@D)/$(genout)
$(genout):
	mkdir $(outdir)
	@touch $@
%/$(genout):
	mkdir $(@D)/$(outdir)
	@touch $@
#

# build and config stuff: (could make this generic default rule...)
#
%/.build:
	@echo 0 >$@
$(flower-config): $(flower-dir)/$(genout)
	touch $@
$(lily-config): $(lib-dir)/$(genout)
	@echo "#define LIBDIR \"./\"" >$@
%.hh:
	touch $@
#

# specific stuff:
#
$(LIBFLOWER): check-flower-version
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/flower/lib
#
$(LIBLILY): dummy
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/lib
#

