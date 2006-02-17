.SUFFIXES: .cc .dep .hh .ll .o .so .yy

$(outdir)/%.o: %.cc
	$(DO_O_DEP) $(CXX) -c $(ALL_CXXFLAGS) -o $@ $<

$(outdir)/%.o: $(outdir)/%.cc
	$(DO_O_DEP) $(CXX) -c $(ALL_CXXFLAGS) -o $@ $<

$(outdir)/%.lo: %.cc
	$(DO_LO_DEP) $(CXX) -c $(ALL_CXXFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.lo: $(outdir)/%.cc
	$(DO_LO_DEP) $(CXX) -c $(ALL_CXXFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.cc $(outdir)/%.hh: %.yy
	$(BISON) -d -o $(subst .hh,.cc,$@)  $<

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -o$@ $<

$(outdir)/%-rc.o: $(outdir)/%.rc
	$(WINDRES) $(WINDRES_FLAGS) -o$@ $<
