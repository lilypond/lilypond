.SUFFIXES: .doc .tely .texi .ly


$(outdir)/%.latex:  %.doc
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BINARY) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir)  $(LILYPOND_BOOK_FLAGS) $<

ifeq ($(out),www)

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BINARY) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<


$(outdir)/%.texi: $(outdir)/%.tely
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BINARY) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<
#
# DON'T REMOVE SOURCE FILES, otherwise the .TEXI ALWAYS OUT OF DATE.
#	rm -f $<

else # out != www

# nexi: n[o-lilypond t]exi
# for plain info doco: don't run lilypond
$(outdir)/%.nexi: %.tely
	rm -f $(outdir)/$*.texi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) --process='true' $<
	mv -f $(outdir)/$*.texinfo $@ 2>/dev/null || mv -f $(outdir)/$*.texi $@

$(outdir)/%.info: $(outdir)/%.nexi
	$(MAKEINFO) -I $(outdir) --output=$(outdir)/$(*F).info $<

endif # out != www

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
