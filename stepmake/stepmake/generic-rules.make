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

