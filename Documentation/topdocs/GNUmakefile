depth = ../..


STEPMAKE_TEMPLATES=documentation tex texinfo topdocs
LOCALSTEPMAKE_TEMPLATES=lilypond ly
HTML_FILES=$(TEXI_FILES:%.texi=$(outdir)/%.html) $(TELY_FILES:%.tely=$(outdir)/%.html)
PDF_FILES=$(outdir)/NEWS.pdf
include $(depth)/make/stepmake.make

MAKEINFO_FLAGS += -I $(top-src-dir)/Documentation/user

ifneq ($(out),www)
LILYPOND_BINARY='true'
endif

$(outdir)/NEWS.nexi: NEWS.tely


