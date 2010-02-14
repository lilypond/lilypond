# rules for directories with html files.

LILYBOOK_HTML_FILES = $(call src-wildcard,*.html)
LILYBOOK_XML_FILES = $(call src-wildcard,*.xml)
LILYBOOK_LYTEX_FILES = $(call src-wildcard,*.lytex)
LILYBOOK_LATEX_FILES = $(call src-wildcard,*.latex)
LILYBOOK_TEX_FILES = $(call src-wildcard,*.tex)

LILYBOOK_OUT_HTML_FILES = ${LILYBOOK_HTML_FILES:%.html=$(outdir)/%.html}
LILYBOOK_OUT_XML_FILES = ${LILYBOOK_XML_FILES:%.xml=$(outdir)/%.html}
LILYBOOK_OUT_LYTEX_FILES = ${LILYBOOK_LYTEX_FILES:%.lytex=$(outdir)/%.pdf}
LILYBOOK_OUT_LATEX_FILES = ${LILYBOOK_LATEX_FILES:%.latex=$(outdir)/%.pdf}
LILYBOOK_OUT_TEX_FILES = ${LILYBOOK_TEX_FILES:%.tex=$(outdir)/%.pdf}

LILYBOOK_OUT_FILES = $(sort $(LILYBOOK_OUT_HTML_FILES) \
                            $(LILYBOOK_OUT_XML_FILES) \
                            $(LILYBOOK_OUT_LYTEX_FILES) \
                            $(LILYBOOK_OUT_LATEX_FILES) \
                            $(LILYBOOK_OUT_TEX_FILES))

EXTRA_DIST_FILES += $(LILYBOOK_HTML_FILES)
