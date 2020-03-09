# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .1 .data .html .gif .png .tex .txt .xpm

$(outdir)/%.gif: %.xpm
	$(call ly_progress,Making,$@,< xpm)
	xpmtoppm $< | ppmtogif > $@

$(outdir)/%.png: %.xpm
	$(call ly_progress,Making,$@,< xpm)
	xpmtoppm $< | pnmtopng > $@
