depth = ../..


STEPMAKE_TEMPLATES=documentation tex texinfo topdocs
LOCALSTEPMAKE_TEMPLATES=lilypond ly
HTML_FILES=$(addprefix $(outdir)/, $(TEXI_FILES:.texi=.html) $(TELY_FILES:.tely=.html))

include $(depth)/make/stepmake.make 

ifneq ($(out),www)
LILYPOND='true'
endif

$(outdir)/NEWS.nexi: NEWS.tely
