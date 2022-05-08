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
	cd $(outdir) && $(buildscript-dir)/run-and-check.sh \
		"$(PDFLATEX) -interaction=nonstopmode -halt-on-error \
			-output-directory=$*.build \
			$(notdir $<)" \
		"$*.pdflatex.log"
	mv $(outdir)/$*.build/$*.pdf $@
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

$(outdir)/%.html: $(outdir)/%.texi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh "DEPTH=$(depth) \
		$(TEXI2HTML_PROGRAM) $(TEXI2HTML_FLAGS) \
		--output=$@ $<"  "$(outdir)/$*.texilog.log"

$(outdir)/%.pdf: $(outdir)/%.texi
	TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
		$(buildscript-dir)/run-and-check.sh \
			"cd $(outdir); \
				texi2pdf --batch $(TEXI2PDF_FLAGS) \
					$(TEXI2PDF_QUIET) \
					-I $(abs-src-dir) \
					$(TEXINFO_PAPERSIZE_OPTION) \
					-o $*.pdf \
					$(<F) \
					< /dev/null" \
			"$(outdir)/$*.texi2pdf.log"

############## DocBook ######################

$(outdir)/%.xml: %.lyxml
	$(call ly_progress,Making,$@,< lyxml)
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(dir $@) $<

# Add the xml => pdf rule only if we have dblatex
ifeq (,$(findstring dblatex,$(MISSING_OPTIONAL)))
$(outdir)/%.pdf: $(outdir)/%.xml
	$(call ly_progress,Making,$@,< xml)
	cd $(outdir) && $(buildscript-dir)/run-and-check.sh \
		"$(DBLATEX) $(DBLATEX_BACKEND) -o $*.pdf $(notdir $<)" \
		"$*.dblatex.log"
endif
