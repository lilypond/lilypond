# init/Makefile

depth = ..

INI_FILES = $(FLY_FILES) $(LY_FILES)
SCM_FILES = $(wildcard *.scm)
EXTRA_DIST_FILES = $(SCM_FILES)

INSTALLATION_DIR=$(datadir)/ly/
INSTALLATION_FILES=$(INI_FILES)

STEPMAKE_TEMPLATES=install
LOCALSTEPMAKE_TEMPLATES=mutopia

include $(depth)/make/stepmake.make 

