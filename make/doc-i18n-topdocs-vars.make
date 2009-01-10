OUTTXT_FILES= $(outdir)/NEWS.txt

TELY_FILES := $(call src-wildcard,*.tely)
MASTER_TEXI_FILES := $(TELY_FILES:%.tely=$(outdir)/%.texi)
HTML_FILES := $(TELY_FILES:%.tely=$(outdir)/%.html)
PDF_FILES := $(TELY_FILES:%.tely=$(outdir)/%.pdf)

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation/user \
  -I $(top-build-dir)/Documentation/user/$(outdir)

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --force --enable-encoding $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

# texi2html flags
TEXI2HTML_INIT= --init-file=$(top-src-dir)/lilypond-texi2html.init
TEXI2HTML_LANG=--lang=$(ISOLANG)
TEXI2HTML_FLAGS += $(TEXI2HTML_LANG) $(DOCUMENTATION_INCLUDES)
TEXI2HTML = LANG= $(TEXI2HTML_PROGRAM)

TEXI2PDF_FLAGS += --batch $(DOCUMENTATION_INCLUDES)

ifdef QUIET_BUILD
TEXI2PDF_FLAGS += -q
endif
