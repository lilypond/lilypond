# init/Makefile

depth = ..

INI_FILES = $(FLY_FILES) $(LY_FILES)
PY_FILES=$(wildcard *.py)
EXTRA_DIST_FILES = $(SCM_FILES)  $(PY_FILES)

INSTALLATION_DIR=$(datadir)/ly/
INSTALLATION_FILES=$(INI_FILES)

STEPMAKE_TEMPLATES=install
LOCALSTEPMAKE_TEMPLATES=mutopia

include $(depth)/make/stepmake.make 

