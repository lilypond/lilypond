# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .1 .data .html .gif .png .tex .txt .xpm

$(outdir)/%.gif: %.xpm
	xpmtoppm $< | ppmtogif > $@

$(outdir)/%.png: %.xpm
	xpmtoppm $< | pnmtopng > $@

# use striproff?
$(outdir)/%.txt: $(outdir)/%.1
	troff -man -Tascii $< | grotty -b -u -o > $@

