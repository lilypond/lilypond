# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex .tely .texi .ly


$(outdir)/%.latex: %.doc
	rm -f $@
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES)\
$(LILYPOND_BOOK_FLAGS) --verbose --dependencies --outdir=$(outdir) $<
	chmod -w $@

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely
	rm -f $@
	set|egrep '(TEX|LILY)'  # ugh, what's this?
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --dependencies --outdir=$(outdir) --format=texi --verbose $(LILYPOND_BOOK_FLAGS) $<
	chmod -w $@

$(outdir)/%.texi: $(outdir)/%.tely
	rm -f $@
	set|egrep '(TEX|LILY)'  # ugh, what's this?
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --dependencies --outdir=$(outdir) --format=texi --verbose $(LILYPOND_BOOK_FLAGS) $<
	rm -f $<
	chmod -w $@

# nexi: no-lily texi
# for plain info doco: don't run lily
$(outdir)/%.nexi: %.tely
	rm -f $@
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --dependencies --outdir=$(outdir) --format=texi --no-lily $(LILYPOND_BOOK_FLAGS) $<
	mv $(@D)/$(*F).texi $@
	chmod -w $@

# nfo: info from non-lily texi
$(outdir)/%.info: $(outdir)/%.nexi
	$(MAKEINFO) --output=$(outdir)/$(*F).info $<

# nfo: info from non-lily texi
#$(outdir)/%.nfo: $(outdir)/%.nexi
#	$(MAKEINFO) --output=$(outdir)/$(*F).info $<

$(outdir)/%.tex: $(outdir)/%.ly
	$(LILYPOND) $(LILYPOND_BOOK_INCLUDES) -o $@ $< 

#
# Timothy's booklet
#
$(outdir)/%-book.ps: $(outdir)/%.ps
	psbook $< $<.tmp
	pstops  '2:0L@.7(21cm,0)+1L@.7(21cm,14.85cm)' $<.tmp $@

$(outdir)/%.pdf: $(outdir)/%.dvi
	dvips -u lilypond.map -o $@.pdfps  -Ppdf $<
	ps2pdf $@.pdfps $@

