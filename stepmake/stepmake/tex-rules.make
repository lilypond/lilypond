
$(outdir)/%.tex: %.tex
	cp $< $@

$(outdir)/%.dvi: $(outdir)/%.tex
	(cd $(outdir); tex \\nonstopmode \\input $(<F))

$(outdir)/%.dvi: $(outdir)/%.latex
	(cd $(outdir)&& \
	  latex \\nonstopmode \\input $(<F)&&\
	  (bibtex $(basename $(<F)) || true) && \
	  latex \\nonstopmode \\input $(<F)&&\
	  (makeindex $(basename $(<F)) || true) && \
	  latex \\nonstopmode \\input $(<F) )

$(outdir)/%.ps: $(outdir)/%.dvi
	cd $(outdir) && dvips -ta4 -o $(@F) $(<F)

$(outdir)-$(PAPERSIZE)/%.ps: $(outdir)-$(PAPERSIZE)/%.dvi
	cd $(outdir) && dvips -t$(PAPERSIZE) -o $(@F) $(<F)

