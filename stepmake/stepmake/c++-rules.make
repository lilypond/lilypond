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
	mv $<.tab.c $@

$(outdir)/%.hh: %.yy
	$(BISON) -d $<
	mv $<.tab.h $@
	rm $<.tab.c		# if this happens in the wrong order it triggers recompile of the .cc file 

$(outdir)/%.cc: %.ll
	$(FLEX) -Cfe -p -p -t $< > $@
