# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex .tely .texi

$(outdir)/%.latex: %.doc
	cd $(outdir);  $(PYTHON) $(depth)/../scripts/mudela-book.py -I .. -I $(depth)/../input/test/ --dependencies ../$< 
#	sed 's! \.\./! !g'<  $(basename $@).dep > $(outdir)/temp.dep 
#	sed 's!^\(.*\):!'$(outdir)'/\1:!g' < $(outdir)/temp.dep > $(basename $@).dep 
#	rm $(outdir)/temp.dep

$(outdir)/%.texi: %.tely
	cd $(outdir); $(PYTHON) $(depth)/../scripts/mudela-book.py -I .. -I $(depth)/../input/test/ --dependencies --format=texi ../$<

