TITLE=LYs Doc
AUTHOR=The LilyPond development team

ifeq ($(out),test)
LILYPOND_BOOK_FLAGS += --use-source-file-names --skip-png-check
endif
