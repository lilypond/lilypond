
default: local-doc 

copy-to-top:  $(TO_TOP_FILES)
	$(foreach i, $(TO_TOP_FILES), \
	  cp $(i) $(builddir) && ) true
	-cp $(outroot)/out-www/*png $(outroot)/out-www/index.html $(builddir)
	-cp $(outdir)/*png $(outdir)/index.html $(builddir)  # don't fail when not making website

local-WWW: $(HTML_FILES) copy-to-top
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

