# title	   generic make rules
# file	   make/Rules.make

# this is supposed to clear all suffixes:
.SUFFIXES:



$(outdir)/%: %.m4
	$(M4) $< > $@

%.dep:
	touch $@

%.gz: %
	gzip -c9 $< > $@

# bit docrules, these...
$(outdir)/%.dvi: $(outdir)/%.tex
	(cd $(outdir); tex \\nonstopmode \\input $(<F))

$(outdir)/%.dvi: $(outdir)/%.latex
	(cd $(outdir); \
	  latex \\nonstopmode \\input $(<F);\
	  bibtex $(basename $(<F));\
	  latex \\nonstopmode \\input $(<F);\
	  latex \\nonstopmode \\input $(<F);\
	)

$(outdir)/%.ps: $(outdir)/%.dvi
	dvips -ta4 -o $@ $<

