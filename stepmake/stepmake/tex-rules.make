
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

$(outdir)/%.pdf: $(outdir)/%.dvi
	cd $(outdir) \
		&& dvips $(DVIPS_FLAGS) -t $(DVIPS_PAPERSIZE) \
			-o $(@F).pdfps $(<F) \
	 	&& gs -dCompatibilityLevel=1.2\
			-sPAPERSIZE=a4\
			-q\
			-dNOPAUSE\
			-dBATCH\
			-sDEVICE=pdfwrite\
			-sOutputFile=$(@F)\
			-dCompatibilityLevel=1.2\
			-sPAPERSIZE=a4\
			-c .setpdfwrite\
			-f $(@F).pdfps


# without -dSAFER
# gs 8.15 complains of safety of loading a ttf directly


