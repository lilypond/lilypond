depth = ../../..

TEMPLATES = lilypond ly lysdoc 

# Using absolute paths in the file names is important for in-tree builds in
# this directory because the tests here use Guile's `(current-filename)` to
# construct an include path dynamically, and relative paths do not provide the
# information they need.  If you are thinking about changing this, be careful.
# CI uses an out-of-tree build, so you could introduce a build error that CI
# won't detect.
COLLATED_FILES = $(sort $(LY_FILES))
NEED_ABSOLUTE_PATHS=YES

# Running these tests in a single lilypond process is an important part of the
# test.
override CPU_COUNT := 1

include $(depth)/make/lilypond.make

TITLE=Path-modification regression tests
