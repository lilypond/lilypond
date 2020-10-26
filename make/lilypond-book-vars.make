# rules for directories with html files.

ifndef VERBOSE
LILYPOND_BOOK_GS_QUIET = -q
endif

LILYPOND_BOOK_COMMAND = LILYPOND_VERSION=$(TOPLEVEL_VERSION) \
	TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) \
	--process='$(LILYPOND_BOOK_PROCESS) \
	$(LILYPOND_BOOK_LILYPOND_FLAGS)' \
	 --redirect-lilypond-output $(LILYPOND_BOOK_FLAGS)

HTML_FILES = $(call src-wildcard,*.html)
HTMLY_FILES = $(call src-wildcard,*.htmly)
XML_FILES = $(call src-wildcard,*.xml)
LYTEX_FILES = $(call src-wildcard,*.lytex)
LATEX_FILES = $(call src-wildcard,*.latex)
TEX_FILES = $(call src-wildcard,*.tex)
TEXI_FILES = $(call src-wildcard,*.texi)
TEXINFO_FILES = $(call src-wildcard,*.texinfo)
TELY_FILES = $(call src-wildcard,*.tely)
DOCBOOK_FILES = $(call src-wildcard,*.lyxml)

OUT_HTML_FILES = ${HTML_FILES:%.html=$(outdir)/%.html}
OUT_HTMLY_FILES = ${HTMLY_FILES:%.htmly=$(outdir)/%.html}
OUT_XML_FILES = ${XML_FILES:%.xml=$(outdir)/%.html}
# If we have pdflatex, create the pdf, otherwise only the .tex file!
ifeq (,$(findstring pdflatex,$(MISSING_OPTIONAL)))
OUT_LYTEX_FILES = ${LYTEX_FILES:%.lytex=$(outdir)/%.pdf}
OUT_LATEX_FILES = ${LATEX_FILES:%.latex=$(outdir)/%.pdf}
OUT_TEX_FILES = ${TEX_FILES:%.tex=$(outdir)/%.pdf}
else
OUT_LYTEX_FILES = ${LYTEX_FILES:%.lytex=$(outdir)/%.tex}
OUT_LATEX_FILES = ${LATEX_FILES:%.latex=$(outdir)/%.tex}
OUT_TEX_FILES = ${TEX_FILES:%.tex=$(outdir)/%.tex}
endif
OUT_TEXI_FILES = ${TEXI_FILES:%.texi=$(outdir)/%.html}
OUT_TEXINFO_FILES = ${TEXINFO_FILES:%.texinfo=$(outdir)/%.html}
OUT_TELY_FILES = ${TELY_FILES:%.tely=$(outdir)/%.html}
# If we have dblatex, create the pdf, otherwise only the .xml file!
ifeq (,$(findstring dblatex,$(MISSING_OPTIONAL)))
OUT_DOCBOOK_FILES = ${DOCBOOK_FILES:%.lyxml=$(outdir)/%.pdf}
else
OUT_DOCBOOK_FILES = ${DOCBOOK_FILES:%.lyxml=$(outdir)/%.xml}
endif
ifneq (,$(findstring xelatex,$(notdir $(PDFLATEX))))
DBLATEX_BACKEND = -b xetex
else
DBLATEX_BACKEND =
endif

OUT_FILES = $(sort $(OUT_HTML_FILES) \
                            $(OUT_HTMLY_FILES) \
                            $(OUT_XML_FILES) \
                            $(OUT_LYTEX_FILES) \
                            $(OUT_LATEX_FILES) \
                            $(OUT_TEX_FILES) \
                            $(OUT_TEXI_FILES) \
                            $(OUT_TEXINFO_FILES) \
                            $(OUT_TELY_FILES) \
                            $(OUT_DOCBOOK_FILES))
