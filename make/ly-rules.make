.SUFFIXES: .doc .dvi .tely .texi .ly


$(outdir)/%.latex: %.doc
	if [ -f $@ ]; then chmod a+w $@; fi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --verbose $(LILYPOND_BOOK_FLAGS) $<
	chmod -w $@

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely
	if [ -f $@ ]; then chmod a+w $@; fi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) $<
	chmod -w $@

$(outdir)/%.texi: $(outdir)/%.tely
	if [ -f $@ ]; then chmod a+w $@; fi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) $<
#
# DON'T REMOVE SOURCE FILES, otherwise the .TEXI ALWAYS OUT OF DATE.
#	rm -f $<
	chmod -w $@

# nexi: no-lily texi
# for plain info doco: don't run lily
$(outdir)/%.nexi: %.tely
	if [ -f $@ ]; then chmod a+w $@; fi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) --verbose $(LILYPOND_BOOK_FLAGS) --process='true' $<
	mv $(outdir)/$*.texinfo $@ 2>/dev/null || mv $(outdir)/$*.texi $@
	chmod -w $@

# nfo: info from non-lily texi
$(outdir)/%.info: $(outdir)/%.nexi
	$(MAKEINFO) -I $(outdir) --output=$(outdir)/$(*F).info $<

# nfo: info from non-lily texi
#$(outdir)/%.nfo: $(outdir)/%.nexi
#	$(MAKEINFO) --output=$(outdir)/$(*F).info $<

#
# let's not do this: this interferes with the lilypond-book dependency mechanism.
#
##$(outdir)/%.tex: $(outdir)/%.ly
##	$(LILYPOND) $(LILYPOND_BOOK_INCLUDES) -o $@ $< 

#
# Timothy's booklet
#
$(outdir)/%-book.ps: $(outdir)/%.ps
	psbook $< $<.tmp
	pstops '2:0L@.7(21cm,0)+1L@.7(21cm,14.85cm)' $<.tmp $@

$(outdir)/%.pdf: $(outdir)/%.dvi
	dvips -u +lilypond.map -o $@.pdfps -t $(DVIPS_PAPERSIZE)  -Ppdf $<
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
