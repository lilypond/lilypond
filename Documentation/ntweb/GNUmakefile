depth = ../..

STEPMAKE_TEMPLATES=documentation
TO_TOP_FILES=$(addprefix $(outdir)/, $(README_TOP_FILES))
PLAIN_FILES=angels.ly guile.patch

EXTRA_DIST_FILES = $(PLAIN_FILES)


include $(depth)/make/stepmake.make 

default: do-doc dist-plain

$(outdir)/%.html: %.yo
	$(sed-version) < $< > $<.out
	$(YODL2HTML) -doutdir=$(outdir) --live-data=3 -o $@ $<.out
	rm -f $<.out
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) \
	--index=http://www.cs.ruu.nl/people/hanwen/lilypond/index.html \
	--news=$(depth)/NEWS $@

dist-plain: 
	cp $(PLAIN_FILES) $(outdir)

local-WWW:

