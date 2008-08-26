
TEXI_FILES = $(call src-wildcard,*.texi)

ALL_SOURCES += $(TEXI_FILES)

TEXINFO_SOURCES = $(TEXI_FILES)

OUTTXT_FILES += $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.txt))

GENERATE_OMF = $(PYTHON) $(buildscript-dir)/texi2omf.py --format $(1) --location $(webdir)/$(tree-dir)/out-www/$(notdir $(basename $@))  --version $(TOPLEVEL_VERSION) $< > $@

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

MAKEINFO_FLAGS = --enable-encoding
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

ifdef QUIET_BUILD
TEXI2PDF_FLAGS += -q
endif

# info stuff
INFO_INSTALL_FILES = $(wildcard $(addsuffix *, $(INFO_FILES)))
INFO_INSTALL_COMMAND =$(if $(INFO_INSTALL_FILES),\
	$(INSTALLPY) -d $(DESTDIR)$(infodir) ; \
	$(MAKE) INSTALLATION_OUT_DIR=$(infodir) \
		depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_INSTALL_FILES)" \
		-f $(stepdir)/install-out.sub.make,true)
