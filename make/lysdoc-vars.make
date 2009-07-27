TITLE=LYs Doc


ifeq ($(COLLATED_FILES),)
COLLATED_FILES = $(sort $(TEXINFO_SOURCES) $(LY_FILES) $(OUT_LY_FILES) )
endif

CSS_FILES = $(shell ls $(top-src-dir)/Documentation/lilypond*.css)
OUT_CSS_FILES = $(foreach f, $(CSS_FILES), $(outdir)/$(notdir $(f)))
