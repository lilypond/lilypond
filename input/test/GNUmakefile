
depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc
LILYPOND_BOOK_FLAGS = --process="lilypond-bin --header=texidoc -I $(srcdir)/input/regression -e '(ly:set-option (quote internal-type-checking) \#t)'"

## Hmm, would this work?  No -- if we really want examples, move
## to other dir (input/) comes to mind.
## examples = font20 ancient-font
## LOCALSTEPMAKE_TEMPLATES += ly mutopia 

EXTRA_DIST_FILES= 

include $(depth)/make/stepmake.make 

TITLE=LilyPond Tips and Tricks
