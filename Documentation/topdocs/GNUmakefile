depth = ../..


STEPMAKE_TEMPLATES=documentation tex texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly
HTML_FILES=$(addprefix $(outdir)/, $(TEXI_FILES:.texi=.html) $(TELY_FILES:.tely=.html))

#default: local-doc 

include $(depth)/make/stepmake.make 

copy-to-top:  $(TO_TOP_FILES)
	$(foreach i, $(TO_TOP_FILES), \
	  cp $(i) $(depth)/ && ) true
	-cp $(outdir)/*png $(outdir)/index.html $(depth)  # don't fail if not making website

###local-WWW: copy-to-top

local-WWW: $(HTML_FILES) copy-to-top
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

