# title	   C++ rules
# file	   make/C++_rules.make

.SUFFIXES: .cc .o .hh .yy .ll  .dep

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
	mv $<.tab.c $(basename $@).cc

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -t $< > $@
# could be faster:
#	$(FLEX) -8 -Cf -t $< > $@

