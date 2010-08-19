depth = ../..
LOCALSTEPMAKE_TEMPLATES = ly
SUBDIRS = ly-examples server
include $(depth)/make/stepmake.make

OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))
EXTRA_DIST_FILES= $(BIB_FILES) $(call src-wildcard,*.bst)
BIB_FILES= $(call src-wildcard,*.bib)

