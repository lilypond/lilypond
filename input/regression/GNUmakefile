depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc

include $(depth)/make/stepmake.make
TITLE=LilyPond Regression Tests

local-clean: local-delete

# use `find` together with `xargs` to avoid too long argument-lists.
local-delete:
	$(FIND) $(outdir)/ -empty -name '*' | xargs rm -f

