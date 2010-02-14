# rules for directories with html files.

LILYBOOK_HTML_FILES = $(call src-wildcard,*.html)
LILYBOOK_XML_FILES = $(call src-wildcard,*.xml)

LILYBOOK_OUT_HTML_FILES = ${LILYBOOK_HTML_FILES:%.html=$(outdir)/%.html}
LILYBOOK_OUT_XML_FILES = ${LILYBOOK_XML_FILES:%.xml=$(outdir)/%.html}

LILYBOOK_OUT_FILES = $(sort $(LILYBOOK_OUT_HTML_FILES) $(LILYBOOK_OUT_XML_FILES))

EXTRA_DIST_FILES += $(LILYBOOK_HTML_FILES)
