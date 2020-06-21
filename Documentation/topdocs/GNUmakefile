depth = ../..

STEPMAKE_TEMPLATES=texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly
OUT_TEXI_FILES = $(TEXI_FILES:%.texi=$(outdir)/%.texi) $(TELY_FILES:%.tely=$(outdir)/%.texi)
HTML_FILES=$(TEXI_FILES:%.texi=$(outdir)/%.html) $(TELY_FILES:%.tely=$(outdir)/%.html)
README_TOP_FILES=NEWS AUTHORS INSTALL README
OUTTXT_FILES= $(outdir)/NEWS.txt

include $(depth)/make/stepmake.make

DOCUMENTATION_INCLUDES += -I $(outdir) \
  -I $(top-src-dir)/Documentation

# FIXME ad-hoc file copy?
$(outdir)/NEWS.tely: $(top-src-dir)/Documentation/changes.tely
	$(call ly_progress,Making,$@,(copy))
	cp -f $< $@

ifeq ($(out),www)
local-WWW-1: $(OUT_TEXI_FILES)
endif
