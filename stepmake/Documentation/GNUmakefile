# Documentation/Makefile

depth = ..


OUTTXT_FILES = $(OUTYO_FILES:.yo=.txt) $(OUTIN_FILES:.yo=.txt)
EXTRA_DIST_FILES = 
SUBDIRS=topdocs


STEPMAKE_TEMPLATES=documentation

include $(depth)/make/stepmake.make 

default: do-doc

do-doc: $(OUTTXT_FILES)

# ugh
check-doc-deps: do-doc
	true

doc: do-doc

