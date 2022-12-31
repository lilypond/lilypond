TEXI_FILES = $(call src-wildcard,*.texi)

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

###########

ifndef VERBOSE
  TEXI2PDF_QUIET = -q
  TEXINFO_GS_QUIET = -q
endif

###########

TEXI2HTML_INIT = \
  --init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init
TEXI2HTML_INCLUDES += --I=$(src-dir) --I=$(outdir)

# The other `TEXI2HTML*` variables are defined in `generic-vars.make`
# because the setup in `Documentation/GNUmakefile` differs too much to
# include this template.
#
# The same holds for the `MAKEINFO*` variables.
