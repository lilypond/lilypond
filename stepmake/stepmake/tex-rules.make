

$(outdir)/%.dvi: $(outdir)/%.tex
	(cd $(outdir); tex \\nonstopmode \\input $(<F))

$(outdir)/%.dvi: $(outdir)/%.latex
	(cd $(outdir)&& \
	  latex \\nonstopmode \\input $(<F)&&\
	  (bibtex $(basename $(<F) || true)&&\
	  latex \\nonstopmode \\input $(<F)&&\
	  latex \\nonstopmode \\input $(<F) ))

$(outdir)/%.ps: $(outdir)/%.dvi
	dvips -ta4 -o $@ $<

