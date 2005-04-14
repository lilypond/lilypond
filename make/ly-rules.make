.SUFFIXES: .doc .dvi .tely .texi .ly


$(outdir)/%.latex $(outdir)/%.fonts.ps:  %.doc
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --verbose $(LILYPOND_BOOK_FLAGS) $<

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely
	rm -f $$(grep -LF '% eof' $(outdir)/lily-*systems.tex 2>/dev/null)
	$(PYTHON) $(LILYPOND_BOOK) --psfonts=$(basename $<).fonts.ps $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) $<

$(outdir)/%.texi: $(outdir)/%.tely
	rm -f $$(grep -LF '% eof' $(outdir)/lily-*systems.tex 2>/dev/null)
	$(PYTHON) $(LILYPOND_BOOK) --psfonts=$(notdir $(basename $<)).fonts.ps $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) $<
#
# DON'T REMOVE SOURCE FILES, otherwise the .TEXI ALWAYS OUT OF DATE.
#	rm -f $<

# nexi: n[o-lilypond t]exi
# for plain info doco: don't run lilypond
$(outdir)/%.nexi: %.tely
	rm -f $(outdir)/$*.texi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) --process='true' $<
	mv $(outdir)/$*.texinfo $@ 2>/dev/null || mv $(outdir)/$*.texi $@

$(outdir)/%.info: $(outdir)/%.nexi
	$(MAKEINFO) -I $(outdir) --output=$(outdir)/$(*F).info $<

$(outdir)/%.html.omf: %.tely
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: %.tely
	$(call GENERATE_OMF,pdf)

$(outdir)/%.ps.gz.omf: %.tely
	$(call GENERATE_OMF,ps.gz) 

$(outdir)/%.html.omf: $(outdir)/%.texi
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: $(outdir)/%.texi
	$(call GENERATE_OMF,pdf)

$(outdir)/%.ps.gz.omf: $(outdir)/%.texi
	$(call GENERATE_OMF,ps.gz) 
