# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex .tely .texi

SUBST_TEXI_DEPS=sed 's! \.\./! !g' < $(basename $@).dep > $(outdir)/temp.dep ; 	mv $(outdir)/temp.dep $(basename $@).dep 

$(outdir)/%.latex: %.doc
	LILYPONDPREFIX=$(LILYPONDPREFIX)/..  $(PYTHON) $(script-dir)/mudela-book.py --outdir=$(outdir) -I .. -I $(input-dir)/test/ --dependencies --dep-prefix=$(outdir)/ $< 
	$(SUBST_TEXI_DEPS)

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --scrdir builds
$(outdir)/%.texi: %.tely
	LILYPONDPREFIX=$(LILYPONDPREFIX)/..  $(PYTHON) $(script-dir)/mudela-book.py --outdir=$(outdir) -I .. -I $(input-dir)/test/ --dependencies --dep-prefix=$(outdir)/ --format=texi $<
	$(SUBST_TEXI_DEPS)

# nexi: no-lily texi
# for plain info doco: don't run lily
$(outdir)/%.nexi: %.tely
	LILYPONDPREFIX=$(LILYPONDPREFIX)/..  $(PYTHON) $(script-dir)/mudela-book.py --outdir=$(outdir) --no-lily -I .. -I $(input-dir)/test/ --dependencies --dep-prefix=$(outdir)/ --format=texi $<
	mv $(@D)/$(*F).texi $@
	$(SUBST_TEXI_DEPS)

$(outdir)/%.info: $(outdir)/%.nexi
	-$(MAKEINFO) --force --output=$@ $<
