.SUFFIXES: .cc .dep .hh .ll .o .so .yy

$(outdir)/%.o: %.cc
	$(DO_O_DEP) $(CXX) -c $(CXXFLAGS) -o $@ $<

$(outdir)/%.o: $(outdir)/%.cc
	$(DO_O_DEP) $(CXX) -c $(CXXFLAGS) -o $@ $<

$(outdir)/%.lo: %.cc
	$(DO_LO_DEP) $(CXX) -c $(CXXFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.lo: $(outdir)/%.cc
	$(DO_LO_DEP) $(CXX) -c $(CXXFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.cc: %.yy
	$(BISON) $<
	@-mv -f $(*F).yy.tab.c $(*F).tab.cc  # bison < 1.30
	mv $(*F).tab.cc $@

$(outdir)/%.hh: %.yy
	$(BISON) -d $<
	@-mv -f $(*F).yy.tab.h $(*F).tab.hh  # bison < 1.30
	mv $(*F).tab.hh $@
	rm -f $(*F).tab.c $(*F).tab.cc	# if this happens in the wrong order it triggers recompile of the .cc file 

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -t $< > $@
