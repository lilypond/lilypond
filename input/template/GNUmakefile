depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LILYPOND_BOOK_FLAGS = --process="lilypond-bin -I $(srcdir)/input/regression -e '(ly:set-option (quote internal-type-checking) \#t)'"
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc
EXTRA_DIST_FILES=README

include $(depth)/make/stepmake.make 

TITLE=LilyPond Template Files

