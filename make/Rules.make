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

.SUFFIXES: .cc .o .hh .y .l  .dep


# compile rules:
#
$(outdir)/%.o: %.cc
	$(DO_CXX_COMPILE)

$(outdir)/%.o: $(outdir)/%.cc
	$(DO_CXX_COMPILE)

$(outdir)/%.cc: %.yy
	$(BISON) $<
	mv $<.tab.c $@

$(outdir)/%.hh: %.yy
	$(BISON) -d $<
	mv $<.tab.h $@
	mv $<.tab.c $(outdir)/$(shell basename $@ .hh).cc

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -t $< > $@
# could be faster:
#	$(FLEX) -8 -Cf -t $< > $@

$(outdir)/%: %.m4
	$(M4) $< > $@

# urg
$(outdir)/%.ly: %.lym4
	$(M4) $< | sed "s/\`/,/g" > $@

# outdirs:
#
# ?$(outdir)/%.dep:
%.dep:
	touch $@

%.gz: %
	gzip -c9 $< > $@

$(depth)/%$(DOTTEXT): check-doc-deps
	rm -f $@
	ln `find ${depth}/Documentation -name ${@F} -print|head -1 ` $@

$(outdir)/%.ps: $(outdir)/%.dvi
	dvips -o $@ $<

