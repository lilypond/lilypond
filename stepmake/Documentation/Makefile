# Documentation/Makefile

depth = ..

include $(depth)/make/Stepmake.make 

OUTTXT_FILES = $(OUTYO_FILES:.yo=.txt) $(OUTIN_FILES:.yo=.txt)
EXTRA_DIST_FILES = 
SUBDIRS=topdocs


include $(stepdir)/Documentation.vars.make
include $(stepdir)/Documentation_rules.make

default: do-doc

do-doc: $(OUTTXT_FILES)

# ugh
check-doc-deps: do-doc
	true

doc: do-doc

