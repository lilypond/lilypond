depth = ../../..

TEMPLATES = lilypond ly lilypond-book

include $(depth)/make/lilypond.make

TITLE=lilypond-book Test Suite

OUT_TEXI_FILES += ${TEXI_FILES:%.texi=$(outdir)/%.info}
OUT_TEXI_FILES += ${TEXI_FILES:%.texi=$(outdir)/%.pdf}
OUT_TEXINFO_FILES += ${TEXINFO_FILES:%.texinfo=$(outdir)/%.info}
OUT_TEXINFO_FILES += ${TEXINFO_FILES:%.texinfo=$(outdir)/%.pdf}
OUT_TELY_FILES += ${TELY_FILES:%.tely=$(outdir)/%.info}
OUT_TELY_FILES += ${TELY_FILES:%.tely=$(outdir)/%.pdf}

XML_FILES = $(filter-out include%,$(call src-wildcard,*.xml))

local-test: $(outdir)/texinfo-filtered.texi
	$(MAKE) LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb $(OUT_FILES)

$(outdir)/texinfo-filtered.texi: texinfo-filter.tely
	cp $< $(basename $@).tely
	$(PYTHON) $(LILYPOND_BOOK) --filter='sed s/a.b.c/d4e4f4/' \
		-o $(dir $@) $(basename $@).tely
	sed 's/a.b.c/d4e4f4/' < $^ > $@.want
	diff $@ $@.want

############## HTML #########################

$(outdir)/%.html: %.html
	$(call ly_progress,Making,$@,< html)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.html: %.htmly
	$(call ly_progress,Making,$@,< htmly)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.html: %.xml
	$(call ly_progress,Making,$@,< xml)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

############## LaTeX ########################

# Only for the following recipe: Prepend the source dir to $TEXINPUTS
# to allow testing `\input` statements that work only with help of
# kpathsea/kpsewhich

$(outdir)/%.tex: %.lytex
	$(call ly_progress,Making,$@,< lytex)
	TEXINPUTS=$(src-dir):$$TEXINPUTS \
		$(LILYPOND_BOOK_COMMAND) --pdf --latex-program $(PDFLATEX) \
		-o $(outdir) $<

$(outdir)/%.tex: %.tex
	$(call ly_progress,Making,$@,< tex)
	$(LILYPOND_BOOK_COMMAND) --pdf --latex-program $(PDFLATEX) \
		-o $(dir $@) $<

$(outdir)/%.tex: %.latex
	$(call ly_progress,Making,$@,< latex)
	$(LILYPOND_BOOK_COMMAND) --pdf --latex-program $(PDFLATEX) \
		-o $(outdir) $<

# Add the tex => pdf rule only if 'pdflatex' is set
ifeq (,$(findstring pdflatex,$(MISSING_OPTIONAL)))
$(outdir)/%.pdf: $(outdir)/%.tex
	$(call ly_progress,Making,$@,< tex)
	rm -fr $(outdir)/$*.build/
	mkdir $(outdir)/$*.build
	TEX="$(PDFTEX)" PDFTEX="$(PDFTEX)" PDFLATEX="$(PDFLATEX)" \
		$(buildscript-dir)/run-and-check.sh \
			"cd $(outdir); \
				$(PDFLATEX) \
					-interaction=nonstopmode \
					-halt-on-error \
					-output-directory=$*.build \
					$(<F)" \
			"$(outdir)/$*.pdflatex.log"
ifeq ($(USE_EXTRACTPDFMARK),yes)
	$(EXTRACTPDFMARK) $(EXTRACTPDFMARK_OPTIONS) \
                 -o $(outdir)/$*.pdfmark $(outdir)/$*.build/$*.pdf
	$(GS920) -dBATCH \
                 -dNOSAFER \
                 -dNOPAUSE \
                 -q \
                 -sDEVICE=pdfwrite \
                 -dAutoRotatePages=/None \
                 -dPrinted=false \
                 -sOutputFile=$@ \
                 -c "30000000 setvmthreshold" \
                 -I $(top-build-dir)/out-fonts \
                 -I $(top-build-dir)/out-fonts/Font \
                 $(outdir)/$*.pdfmark \
                 $(outdir)/$*.build/$*.pdf
else
	mv $(outdir)/$*.build/$*.pdf $@
endif
	rm -fr $(outdir)/$*.build/
endif

############## Texinfo ######################

$(outdir)/%.texi: %.texi
	$(call ly_progress,Making,$@,< texi)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.texi: %.itexi
	$(call ly_progress,Making,$@,< itexi)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.texi: %.texinfo
	$(call ly_progress,Making,$@,< texinfo)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.texi: %.tely
	$(call ly_progress,Making,$@,< tely)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.info: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh \
		"$(TEXI2ANY) --info --output=$@ $<" "$(outdir)/$*.texi2any-info.log"

$(outdir)/%.html: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh "DEPTH=$(depth) \
		$(TEXI2ANY) --html \
		--output=$@ $<"  "$(outdir)/$*.texilog.log"

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

############## DocBook ######################

$(outdir)/%.xml: %.lyxml
	$(call ly_progress,Making,$@,< lyxml)
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(dir $@) $<

# Add the xml => pdf rule only if we have dblatex
ifeq (,$(findstring dblatex,$(MISSING_OPTIONAL)))
$(outdir)/%.pdf: $(outdir)/%.xml
	$(call ly_progress,Making,$@,< xml)
	TEX="$(PDFTEX)" PDFTEX="$(PDFTEX)" PDFLATEX="$(PDFLATEX)" \
		$(buildscript-dir)/run-and-check.sh \
			"cd $(outdir); \
				$(DBLATEX) \
					$(DBLATEX_BACKEND) \
					-o $*.tmp.pdf \
					$(<F)" \
			"$(outdir)/$*.dblatex.log"
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
endif
