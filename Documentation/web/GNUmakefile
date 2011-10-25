depth = ../..

LOCALSTEPMAKE_TEMPLATES = ly

SUBDIRS = server
include $(depth)/make/stepmake.make

#OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))
BIB_FILES = $(call src-wildcard,*.bib)
EXTRA_DIST_FILES += $(BIB_FILES) $(call src-wildcard,*.bst)

