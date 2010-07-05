depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc lilypond-book

ifndef LILYBOOK_OUT_FILES
LILYBOOK_OUT_FILES = ""
endif

COLLATED_FILES = $(LILYBOOK_OUT_FILES)
EXTRA_DIST_FILES = suffix-texinfo.texinfo

include $(depth)/make/stepmake.make

TITLE=lilypond-book Test Suite
