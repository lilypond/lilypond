PO_FILES = $(call src-wildcard,*.po)
MO_FILES = $(addprefix $(outdir)/, $(PO_FILES:%.po=%/LC_MESSAGES/lilypond.mo))

CATALOGS = $(PO_FILES:%.po=%)
