TITLE=LYs Doc
AUTHOR=The LilyPond development team

ifndef COLLATED_FILES
COLLATED_FILES = $(sort $(TEXINFO_SOURCES) $(LY_FILES) $(OUT_LY_FILES) )
endif

ifeq ($(out),test)
LILYPOND_BOOK_FLAGS += --use-source-file-names --skip-png-check
endif
