
$(outdir)/%.tex: %.tex
	$(call ly_progress,Making,$@,(copy))
	cp $< $@

$(outdir)/%.dvi: $(outdir)/%.tex
	$(call ly_progress,Making,$@,< tex)
	(cd $(outdir); tex \\nonstopmode \\input $(<F))

$(outdir)/%.dvi: $(outdir)/%.latex
	$(call ly_progress,Making,$@,< latex)
	(cd $(outdir)&& \
	  latex \\nonstopmode \\input $(<F)&&\
	  (bibtex $(basename $(<F)) || true) && \
	  latex \\nonstopmode \\input $(<F)&&\
	  (makeindex $(basename $(<F)) || true) && \
	  latex \\nonstopmode \\input $(<F) )

$(outdir)/%.pdf: $(outdir)/%.dvi
	$(call ly_progress,Making,$@,< dvi)
	cd $(outdir) \
		&& dvips $(DVIPS_FLAGS) -t $(DVIPS_PAPERSIZE) \
			-o $(@F).pdfps $(<F) \
		&& gs -dCompatibilityLevel=1.2 \
                      -sPAPERSIZE=a4 \
                      -q \
                      -dNOPAUSE \
                      -dBATCH \
                      -sDEVICE=pdfwrite \
                      -dAutoRotatePages=/None \
                      -dPrinted=false \
                      -sOutputFile=$(@F) \
                      -dCompatibilityLevel=1.2 \
                      -sPAPERSIZE=a4 \
                      -c .setpdfwrite \
                      -f $(@F).pdfps


# without -dSAFER
# gs 8.15 complains of safety of loading a ttf directly


