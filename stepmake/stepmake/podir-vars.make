PO_FILES = $(call src-wildcard,*.po)
MO_FILES = $(addprefix $(outdir)/, $(PO_FILES:.po=.mo))

CATALOGS = $(PO_FILES:%.po=%)
