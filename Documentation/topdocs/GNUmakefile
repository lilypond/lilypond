depth = ../..


STEPMAKE_TEMPLATES=documentation tex texinfo yolily-topdoc
LOCALSTEPMAKE_TEMPLATES=lilypond ly
HTML_FILES=$(addprefix $(outdir)/, $(TEXI_FILES:.texi=.html) $(TELY_FILES:.tely=.html))


include $(depth)/make/stepmake.make 

local-WWW: $(HTML_FILES)
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

