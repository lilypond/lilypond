# Generate.make ?

parsheadorig=$(CCDIR)/parser.tab.h
parsheadnew=$(HEADERDIR)/parser.hh

#
# take some trouble to avoid overwriting the old y.tab.h,
# because otherwise all dependants would be remade.
$(CCDIR)/parser.cc: $(CCDIR)/parser.y
	$(BISON) -d $<
	mv $(CCDIR)/parser.tab.c $@
	(if diff  $(parsheadorig) $(parsheadnew)>/dev/null; then \
		echo Ignoring $(parsheadorig);  \
	else \
		mv $(parsheadorig) $(parsheadnew); \
	fi )

$(parsheadnew): $(CCDIR)/parser.cc

$(HEADERDIR)/version.hh: Variables.make make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL) "$(CXX) $(CXXVER)" > $@

$(CCDIR)/%.cc: $(CCDIR)/%.y
	$(BISON) -d $<
	mv $(CCDIR)/$(shell basename $@ .cc ).tab.h $(HEADERDIR)/$(shell basename $@ .cc).hh
	mv $(CCDIR)/$(shell basename $@ .cc ).tab.c $@

$(CCDIR)/%.cc: $(CCDIR)/%.l
	$(FLEX)  -t $< > $@

