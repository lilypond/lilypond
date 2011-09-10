depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc lilypond-book

COLLATED_FILES = $(OUT_FILES)

include $(depth)/make/stepmake.make

TITLE=lilypond-book Test Suite

EXTRA_DIST_FILES = include.mxl
EXTRA_DIST_FILES += $(call src-wildcard,*.ly)
EXTRA_DIST_FILES += $(call src-wildcard,*.ily)
EXTRA_DIST_FILES += $(call src-wildcard,*.tely)
EXTRA_DIST_FILES += $(call src-wildcard,*.itely)


