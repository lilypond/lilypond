# Docrules.make

# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .1 .data .html .gif .xpm .tex .txt

$(outdir)/%.gif: %.xpm
	xpmtoppm $< | ppmtogif > $@

# use striproff?
$(outdir)/%.txt: $(outdir)/%.1
	troff -man -Tascii $< | grotty -b -u -o > $@

#urg should generalise and move Lilypond -> StepMake
$(outdir)/%.html: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --package=$(topdir) -o $@ $<
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $@

$(outdir)/%.tex: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --package=$(topdir) -o $@ --latex $<

include $(stepdir)/yodl-rules.make
include $(stepdir)/texinfo-rules.make

