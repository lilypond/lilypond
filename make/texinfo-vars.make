TEXI_FILES = $(call src-wildcard,*.texi)

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

MAKEINFO_FLAGS += --enable-encoding --error-limit=0 $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG=C $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

###########

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
