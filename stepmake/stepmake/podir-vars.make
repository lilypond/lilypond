# empty

POT_FILES = $(call src-wildcard,*.pot)
PO_FILES = $(call src-wildcard,*.po)
MO_FILES = $(addprefix $(outdir)/, $(PO_FILES:.po=.mo))

HELP_CATALOGS = $(PO_FILES:%.po=%)
CATALOGS = $(HELP_CATALOGS:$(DOMAIN)=)

DIST_FILES += $(POT_FILES) $(PO_FILES)

