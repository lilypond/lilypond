HTML_PAGE_NAMES = index translations
HTML_FILES = $(HTML_PAGE_NAMES:%=%.html)
OUT_HTML_FILES = $(HTML_PAGE_NAMES:%=$(outdir)/%.html)
