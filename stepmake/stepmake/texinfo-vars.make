
TEXI_FILES = $(wildcard *.texi)

ALL_SOURCES += $(TEXI_FILES)

OUTTXT_FILES += $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.txt))
