
default: local-doc 

copy-to-top:  $(TO_TOP_FILES)
	$(foreach i, $(TO_TOP_FILES), \
	  cp $(i) $(builddir) && ) true

local-WWW: $(HTML_FILES) copy-to-top

