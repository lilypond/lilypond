depth = ../..

STEPMAKE_TEMPLATES=documentation
TO_TOP_FILES=$(addprefix $(outdir)/, $(README_TOP_FILES))
PLAIN_FILES=angels.ly guile.patch


include $(depth)/make/stepmake.make 

default: do-doc 

$(outdir)/%.html: %.yo
	$(sed-version) < $< > $<.out
	$(YODL2HTML) -doutdir=$(outdir) --live-data=3 -o $@ $<.out
	rm -f $<.out
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) \
	--index=http://www.cs.ruu.nl/people/hanwen/lilypond/index.html \
	--news=$(depth)/NEWS $@

dist-plain: 
	cp $(PLAIN_FILES) $(out-dir)

local-WWW:

