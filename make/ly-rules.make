.SUFFIXES: .doc .dvi .tely .texi .ly


$(outdir)/%.latex: %.doc
	if [ -f $@ ]; then chmod a+w $@; fi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --verbose $(LILYPOND_BOOK_FLAGS) $<
	chmod -w $@

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely
	if [ -f $@ ]; then chmod a+w $@; fi
	rm -f $$(grep -LF '\lilypondend' out/lily-*.tex 2>/dev/null)
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) $<
	chmod -w $@

$(outdir)/%.texi: $(outdir)/%.tely
	if [ -f $@ ]; then chmod a+w $@; fi
	rm -f $$(grep -LF '\lilypondend' out/lily-*.tex 2>/dev/null)
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) $<
#
# DON'T REMOVE SOURCE FILES, otherwise the .TEXI ALWAYS OUT OF DATE.
#	rm -f $<
	chmod -w $@

# nexi: n[o-lilypond t]exi
# for plain info doco: don't run lilypond
$(outdir)/%.nexi: %.tely
	if [ -f $@ ]; then chmod a+w $@; fi
	rm -f $(outdir)/$*.texi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) --process='true' $<
	mv $(outdir)/$*.texinfo $@ 2>/dev/null || mv $(outdir)/$*.texi $@
# makeinfo <= 4.6 image bug workaround
	if [ $(MAKEINFO_VERSION) -le 4006000 ]; then \
		(cd $(outdir) && \
		ls -1 lily-*.ly | sed 's/.ly$$/.txt/' | xargs touch) || true; \
	fi
	chmod -w $@

$(outdir)/%.info: $(outdir)/%.nexi
	$(MAKEINFO) -I $(outdir) --output=$(outdir)/$(*F).info $<

# Timothy's booklet
#
$(outdir)/%-book.ps: $(outdir)/%.ps
	psbook $< $<.tmp
	pstops '2:0L@.7(21cm,0)+1L@.7(21cm,14.85cm)' $<.tmp $@

$(outdir)/%.pdf: $(outdir)/%.dvi
	dvips $(DVIPS_FLAGS)  -o $@.pdfps -t $(DVIPS_PAPERSIZE)  $<
	ps2pdf -sPAPERSIZE=$(DVIPS_PAPERSIZE) $@.pdfps $@

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
