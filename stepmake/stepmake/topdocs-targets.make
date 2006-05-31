
default: local-doc

copy-to-top:  $(TO_TOP_FILES)
	$(foreach i, $(TO_TOP_FILES), \
	  cp $(i) $(top-build-dir) && ) true

local-WWW: $(HTML_FILES) $(PDF_FILES) copy-to-top

