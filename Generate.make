
parsheadorig=$(CCDIR)/parser.tab.h
parsheadnew=$(HEADERDIR)/parser.hh

#
# take some trouble to avoid overwriting the old y.tab.h
$(CCDIR)/parser.cc: $(CCDIR)/parser.y
	$(BISON) -d $<
	(if diff  $(parsheadorig) $(parsheadnew)>/dev/null; then \
		echo leaving $(parsheadnew);  \
	else \
		mv $(parsheadorig) $(parsheadnew); \
	fi )
	mv $(CCDIR)/parser.tab.c $@

$(parsheadnew): $(CCDIR)/parser.cc

$(HEADERDIR)/version.hh: Variables.make make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL) "$(CXX) $(CXXVER)" > $@

$(CCDIR)/lexer.cc: $(CCDIR)/lexer.l
	$(FLEX)  -t $< > $@




