#
# project  LilyPond -- the musical typesetter
# title	   generic make rules
# file	   make/Rules.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

.SUFFIXES:
.SUFFIXES: .cc .o .hh .y .l .pod .txt .1 .dep

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

$(outdir)/%.1: %.pod
	pod2man --center="LilyPond documentation" --section="0"\
		--release="LilyPond $(MAJVER).$(MINVER).$(PATCHLEVEL)" $< > $@
#

# specific stuff:
#
$(LIBFLOWER): check-flower-version
#	$(MAKE) -C ./$(depth)/flower/lib
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/flower/lib
#
$(LIBLILY): check-flower-version $(lily-version)
#	$(MAKE) -C ./$(depth)/lib
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/lib
#

