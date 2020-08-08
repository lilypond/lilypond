TEXI_FILES = $(call src-wildcard,*.texi)
TEXINFO_SOURCES = $(TEXI_FILES)

OUTTXT_FILES += $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.txt))

# Find the file $(1) within the texinfo include dirs and return its path.
# If not found, return $(outdir)/$(1) assuming that it is a generated file.
find-texi = \
$(firstword \
	$(wildcard $(src-dir)/$(1)) \
	$(wildcard $(top-src-dir)/Documentation/$(1)) \
	$(outdir)/$(1) \
)

# Recursively scan the file $(1) for @include and @verbatiminclude, search for
# included files within the texinfo include dirs, and return all dependencies.
scan-texi = \
$(foreach f, $(shell test -f $(1) && sed -ne "/^@\(verbatim\)\?include[[:space:]]/s/@\(verbatim\)\?include//p" $(1)), \
	$(call find-texi,$(f)) \
	$(call scan-texi,$(call find-texi,$(f))) \
)

# Find dependencies for the target $@, based on the texinfo source file $<,
# and write the dependencies to a .dep file.
DO_TEXI_DEP = ( echo ./$@: $(call scan-texi,$<) > $(basename $@).dep ) &&

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

MAKEINFO_FLAGS += --enable-encoding --error-limit=0 $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG=C $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

###########
ifneq ($(ISOLANG),)
TEXI2HTML_LANG = --document-language=$(ISOLANG)
endif

TEXI2HTML_INIT = --init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init

TEXI2HTML_SPLIT = --prefix=index --split=section

TEXI2HTML_INCLUDES += --I=$(src-dir) --I=$(outdir) $(DOCUMENTATION_INCLUDES)
TEXI2HTML_FLAGS += $(TEXI2HTML_INCLUDES) $(TEXI2HTML_INIT) $(TEXI2HTML_LANG) $(TEXI2HTML_ERROR_LIMIT)

TEXI2HTML = TOP_SRC_DIR=$(top-src-dir) PERL_UNICODE=SD $(TEXI2HTML_PROGRAM)
###########

TEXI2PDF_FLAGS += $(DOCUMENTATION_INCLUDES)

ifndef VERBOSE
  TEXI2PDF_QUIET = -q
  TEXINFO_GS_QUIET = -q
endif

# info stuff
INFO_INSTALL_FILES = $(wildcard $(addsuffix *, $(INFO_FILES)))
INFO_INSTALL_COMMAND = $(if $(INFO_INSTALL_FILES),\
	$(INSTALLPY) -d $(DESTDIR)$(infodir) ; \
	$(MAKE) INSTALLATION_OUT_DIR=$(infodir) \
		depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_INSTALL_FILES)" \
		-f $(stepdir)/install-out.sub.make,true)
