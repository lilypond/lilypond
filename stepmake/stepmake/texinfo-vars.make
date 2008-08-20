TEXI_FILES = $(call src-wildcard,*.texi)

ALL_SOURCES += $(TEXI_FILES)

TEXINFO_SOURCES = $(TEXI_FILES)

OUTTXT_FILES += $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.txt))

GENERATE_OMF = $(PYTHON) $(buildscript-dir)/texi2omf.py --format $(1) --location $(webdir)/$(tree-dir)/out-www/$(notdir $(basename $@))  --version $(TOPLEVEL_VERSION) $< > $@

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

MAKEINFO_FLAGS = --enable-encoding
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

# texi2html xref map files
XREF_MAPS_DIR=$(top-build-dir)/out/xref-maps
XREF_MAPS_FILES=$(INFO_DOCS:%=$(XREF_MAPS_DIR)/%.xref-map)

# texi2html flags
ifneq ($(ISOLANG),) 
TEXI2HTML_LANG = --lang=$(ISOLANG)
endif
TEXI2HTML_FLAGS += --css-ref=lilypond.css $(DOCUMENTATION_INCLUDES) \
  --I=$(XREF_MAPS_DIR)
TEXI2HTML_INIT = --init-file=$(top-src-dir)/lilypond-texi2html.init
TEXI2HTML = $(TEXI2HTML_PROGRAM) $(TEXI2HTML_FLAGS) $(TEXI2HTML_LANG)

TEXI2PDF_FLAGS += -q

# info stuff
INFO_INSTALL_FILES = $(wildcard $(addsuffix *, $(INFO_FILES)))
INFO_INSTALL_COMMAND =$(if $(INFO_INSTALL_FILES),\
	$(INSTALLPY) -d $(DESTDIR)$(infodir) ; \
	$(MAKE) INSTALLATION_OUT_DIR=$(infodir) \
		depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_INSTALL_FILES)" \
		-f $(stepdir)/install-out.sub.make,true)
