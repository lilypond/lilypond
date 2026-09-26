depth = ../..

TEMPLATES = lilypond ly lysdoc 

# these aren't useful as regression tests because they're not repeatable
NON_COLLATED_FILES := \
  adding-the-current-date-to-a-score.ly \
  generating-random-notes.ly \
# end

COLLATED_FILES = $(filter-out $(NON_COLLATED_FILES),$(sort $(LY_FILES)))

include $(depth)/make/lilypond.make

TITLE=Regression tests: Doc Snippets
