# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex .tely .texi

SUBST_TEXI_DEPS=sed 's! \.\./! !g' < $(basename $@).dep > $(outdir)/temp.dep ; 	mv $(outdir)/temp.dep $(basename $@).dep 

$(outdir)/%.latex: %.doc
	rm -f $@
	LILYPONDPREFIX=$(LILYPONDPREFIX)/..  $(PYTHON) $(script-dir)/lilypond-book.py --outdir=$(outdir) -I .. -I $(input-dir)/test/ --dependencies --dep-prefix=$(outdir)/ $<
	chmod -w $@
	$(SUBST_TEXI_DEPS)

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --scrdir builds
$(outdir)/%.texi: %.tely
	rm -f $@
	LILYPONDPREFIX=$(LILYPONDPREFIX)/..  $(PYTHON) $(script-dir)/lilypond-book.py --outdir=$(outdir) -I .. -I $(input-dir)/test/ --dependencies --format=texi $<
	$(SUBST_TEXI_DEPS)
	chmod -w $@

# nexi: no-lily texi
# for plain info doco: don't run lily
$(outdir)/%.nexi: %.tely
	rm -f $@
	LILYPONDPREFIX=$(LILYPONDPREFIX)/..  $(PYTHON) $(script-dir)/lilypond-book.py --outdir=$(outdir) --no-lily -I .. -I $(input-dir)/test/ --dependencies --dep-prefix=$(outdir)/ --format=texi $<
	mv $(@D)/$(*F).texi $@
	$(SUBST_TEXI_DEPS)
	chmod -w $@

# nfo: info from non-lily texi
$(outdir)/%.info: $(outdir)/%.nexi
	-$(MAKEINFO) --force --output=$(outdir)/$(*F).info $<

# nfo: info from non-lily texi
#$(outdir)/%.nfo: $(outdir)/%.nexi
#	-$(MAKEINFO) --force --output=$(outdir)/$(*F).info $<
