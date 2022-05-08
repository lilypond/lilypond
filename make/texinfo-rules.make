
.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%-big-page.html: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh "DEPTH=$(depth) $(TEXI2HTML) $(TEXI2HTML_FLAGS) -D bigpage -D web_version --output=$@ $<"  "$(outdir)/$*.bigtexi.log"

$(outdir)/%.html: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh "DEPTH=$(depth) $(TEXI2HTML) $(TEXI2HTML_FLAGS) --output=$@ $<"  "$(outdir)/$*.texilog.log"

$(outdir)/%/index.html: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	mkdir -p $(dir $@)
	$(buildscript-dir)/run-and-check.sh "DEPTH=$(depth)/../ $(TEXI2HTML) $(TEXI2HTML_SPLIT) $(TEXI2HTML_FLAGS) --output=$(dir $@) $<"  "$(outdir)/$*.splittexi.log"

$(outdir)/%.pdf: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
		$(buildscript-dir)/run-and-check.sh \
			"cd $(outdir); \
				texi2pdf --batch $(TEXI2PDF_FLAGS) \
					$(TEXI2PDF_QUIET) \
					-I $(abs-src-dir) \
					$(TEXINFO_PAPERSIZE_OPTION) \
					-o $*.tmp.pdf \
					$(<F) \
					< /dev/null" \
			"$(outdir)/$*.texi2pdf.log"
ifeq ($(USE_EXTRACTPDFMARK),yes)
	$(EXTRACTPDFMARK) -o $(outdir)/$*.pdfmark $(outdir)/$*.tmp.pdf
	$(GS920) -dBATCH \
                 -dNOSAFER \
                 -dNOPAUSE \
                 $(TEXINFO_GS_QUIET) \
                 -sDEVICE=pdfwrite \
                 -dAutoRotatePages=/None \
                 -dPrinted=false \
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
