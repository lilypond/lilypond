include Variables.make

initdefault: $(CCDIR)/parser.cc $(CCDIR)/lexer.cc hdr/version.hh alldeps

include Generate.make

alldeps: #$(ALLDEPS)
	touch $(DEPDIR)/dummy.dep

$(DEPDIR)/%.dep:  $(CCDIR)/%.cc
	$(DODEP) $(CXX) -E  $(CXXFLAGS) $^ > /dev/null

