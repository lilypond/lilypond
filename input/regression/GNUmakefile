depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'


include $(depth)/make/stepmake.make 
TITLE=LilyPond Regression Tests


local-clean: local-delete

# have to do it in 2 steps to prevent
# argument-list too long.
local-delete:
	rm -f $(outdir)/lily-1*
	rm -f $(outdir)/*

