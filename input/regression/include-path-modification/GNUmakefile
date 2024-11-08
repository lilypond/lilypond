depth = ../../..

TEMPLATES = lilypond ly lysdoc 

COLLATED_FILES = $(sort $(LY_FILES))

# Running these tests in a single lilypond process is an important part of the
# test.
override CPU_COUNT := 1

include $(depth)/make/lilypond.make

TITLE=Path-modification regression tests
