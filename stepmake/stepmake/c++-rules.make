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
	$(BISON) -o $@  $<

$(outdir)/%.hh: %.yy
	$(BISON) -o $(subst .hh,-tmp.cc,$@) -d  $<
	rm $(subst .hh,-tmp.cc,$@)
	mv $(subst .hh,-tmp.hh,$@) $@

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -o$@ $<

$(outdir)/%-rc.o: $(outdir)/%.rc
	$(WINDRES) $(WINDRES_FLAGS) -o$@ $<
