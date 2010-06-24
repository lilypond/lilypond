TITLE=LYs Doc
AUTHOR=Han-Wen Nienhuys and Jan Nieuwenhuizen


ifeq ($(COLLATED_FILES),)
COLLATED_FILES = $(sort $(TEXINFO_SOURCES) $(LY_FILES) $(OUT_LY_FILES) )
endif

ifeq ($(out),test)
LILYPOND_BOOK_FLAGS += --use-source-file-names
endif
