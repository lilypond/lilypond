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
	$(DO_CXX_COMPILE)

$(outdir)/%.o: $(outdir)/%.cc
	$(DO_CXX_COMPILE)

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
	$(FLEX) -Cfe -p -p -t $< > $@

$(outdir)/%.text: $(outdir)/%.1
	groff -man -Tascii $< > $@

$(depth)/%.text: $(outdir)/%.text
	cp $< $@

$(outdir)/%.5: %.pod
	$(pod2groff)
$(outdir)/%.1: %.pod
	$(pod2groff)



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

