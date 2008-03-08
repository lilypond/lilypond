.SUFFIXES: .doc .tely .texi .ly

$(outdir)/%.latex:  %.doc
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_INCLUDES) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir)  $(LILYPOND_BOOK_FLAGS) $<

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_INCLUDES) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<


$(outdir)/%.texi: $(outdir)/%.tely
	echo $(LILYPOND_BOOK_PROCESS)
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_INCLUDES) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<
#
# DON'T REMOVE SOURCE FILES, otherwise the .TEXI ALWAYS OUT OF DATE.
#	rm -f $<


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
