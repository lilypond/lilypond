
TEXI_FILES = $(wildcard *.texi)

ALL_SOURCES += $(TEXI_FILES)

TEXINFO_SOURCES = $(TEXI_FILES)

OUTTXT_FILES += $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.txt))

GENERATE_OMF = $(PYTHON) $(depth)/buildscripts/texi2omf.py --format $(1) --location $(local_package_docdir)/$(current-relative-dir)/out-www/$(notdir $(basename $@))  --version $(TOPLEVEL_VERSION) $< > $@

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

MAKEINFO = LANG= $(MAKEINFO_PROGRAM)

# info stuff
INFO_INSTALL_FILES = $(wildcard $(addsuffix *, $(INFO_FILES)))
INFOINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(DESTDIR)$(package_infodir) depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_INSTALL_FILES)" -f $(stepdir)/install-out.sub.make

