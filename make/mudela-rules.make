# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex .tely .texi

SUBST_TEXI_DEPS=sed 's! \.\./! !g' < $(basename $@).dep > $(outdir)/temp.dep ; 	mv $(outdir)/temp.dep $(basename $@).dep 

$(outdir)/%.latex: %.doc
	cd $(outdir);  $(PYTHON) $(depth)/../scripts/mudela-book.py -I .. -I $(depth)/../input/test/ --dependencies --dep-prefix=$(outdir)/ ../$< 
	$(SUBST_TEXI_DEPS)

$(outdir)/%.texi: %.tely
	cd $(outdir); $(PYTHON) $(depth)/../scripts/mudela-book.py -I .. -I $(depth)/../input/test/ --dependencies --dep-prefix=$(outdir)/ --format=texi ../$<
	$(SUBST_TEXI_DEPS)
