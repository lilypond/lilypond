# Documentation/Makefile

depth = ..


OUTTXT_FILES = $(OUTYO_FILES:.yo=.txt) $(OUTIN_FILES:.yo=.txt)
EXTRA_DIST_FILES = 
OUT_DIST_FILES=$(OUTTXT_FILES)

SUBDIRS=topdocs

STEPMAKE_TEMPLATES=documentation install install-out

include $(depth)/make/stepmake.make 

default: do-doc

do-doc: $(OUTTXT_FILES)

# ugh
check-doc-deps: do-doc
	true

doc: do-doc

INSTALLATION_DIR=$(datadir)/Documentation
INSTALLATION_FILES=$(DIST_FILES)

INSTALLATION_OUT_DIR=$(datadir)/Documentation/out
INSTALLATION_OUT_FILES=$(OUTTXT_FILES)

