$(outdir)/collated-files.list: $(COLLATED_FILES)
	$(call ly_progress,Making,$@,< $(words $^) files)
	@true ; $(file >$@,$^)

$(outdir)/collated-files.tely: $(outdir)/collated-files.list
	$(call ly_progress,Making,$@,)
	$(LYS_TO_TELY) --output=$(outdir)/collated-files.tely \
	               --title="$(TITLE)" \
	               --author="$(AUTHOR)" \
	               --input-filename=$^

# This tells make that $(COLLATED_FILES) must exist in order to build
# this target, i.e. they can't be treated as intermediates.
$(outdir)/collated-files.texi: $(COLLATED_FILES)

.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.html: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh "DEPTH=$(depth) $(TEXI2ANY_HTML) --no-split --output=$@ $<"  "$(outdir)/$*.texi2any-html.log"

$(outdir)/%.pdf: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	TEX="$(PDFTEX)" PDFTEX="$(PDFTEX)" PDFLATEX="$(PDFLATEX)" \
		$(buildscript-dir)/run-and-check.sh \
			"cd $(outdir); \
				texi2pdf \
					-I $(abs-src-dir) \
					-o $*.tmp.pdf \
					$(<F) \
					< /dev/null" \
			"$(outdir)/$*.texi2pdf.log"
ifeq ($(USE_EXTRACTPDFMARK),yes)
	$(EXTRACTPDFMARK) $(EXTRACTPDFMARK_OPTIONS) \
                 -o $(outdir)/$*.pdfmark $(outdir)/$*.tmp.pdf
	$(GS920) -dBATCH \
                 -dNOSAFER \
                 -dNOPAUSE \
                 -q \
                 -sDEVICE=pdfwrite \
                 -dAutoRotatePages=/None \
                 -dPrinted=false \
                 -dPreserveMarkedContent=true \
                 -sOutputFile=$@ \
                 -c "30000000 setvmthreshold" \
                 -I $(top-build-dir)/out-fonts \
                 -I $(top-build-dir)/out-fonts/Font \
                 $(outdir)/$*.pdfmark \
                 $(outdir)/$*.tmp.pdf
	rm $(outdir)/$*.tmp.pdf
else
	mv $(outdir)/$*.tmp.pdf $@
endif
