.SUFFIXES: .cc .dep .hh .ll .o .so .yy

$(outdir)/%.o: %.cc
	$(DO_O_DEP) $(CXX) -c $(ALL_CXXFLAGS) -o $@ $<

$(outdir)/%.o: $(outdir)/%.cc
	$(DO_O_DEP) $(CXX) -c $(ALL_CXXFLAGS) -o $@ $<

$(outdir)/%.lo: %.cc
	$(DO_LO_DEP) $(CXX) -c $(ALL_CXXFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.lo: $(outdir)/%.cc
	$(DO_LO_DEP) $(CXX) -c $(ALL_CXXFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.cc: %.yy
	$(BISON) -o $@ $<
	-mv -f $(*F).yy.tab.c $@ # bison < 1.30

$(outdir)/%.hh: %.yy
	$(BISON) -o$(outdir)/$(*F).cc -d $<
	-mv -f $(*F).yy.tab.h $@  # bison < 1.30
	-mv $(*F).tab.hh $@
	rm -f $(*F).tab.c $(*F).tab.cc # bison < 1.30
	rm -f $(outdir)/$(*F).cc # avoid recompiling the .cc file 

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -t $< > $@
