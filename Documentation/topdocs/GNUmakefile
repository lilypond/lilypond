depth = ../..

BLURBS=BLURB COPERTINA FLAPTEKST

STEPMAKE_TEMPLATES=documentation tex texinfo yolily-topdoc
LOCALSTEPMAKE_TEMPLATES=lilypond mudela
HTML_FILES=$(addprefix $(outdir)/, $(TEXI_FILES:.texi=.html) $(TELY_FILES:.tely=.html))


include $(depth)/make/stepmake.make 

local-WWW: $(HTML_FILES)

