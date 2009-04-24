depth = ../..

# urg, can't name itely templates foo.itely.in because of standard rule

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly
EXTRA_DIST_FILES += README $(call src-wildcard,*.snippet-list)

default:

ifneq ($(out),)
MAIN_INFO_DOC = lilypond-snippets
INFO_DOCS = lilypond-snippets
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)
INFO_IMAGES_DIR = lilypond-snippets
DEST_INFO_IMAGES_SUBDIR = input/lsr
endif

include $(depth)/make/stepmake.make

TEXI2HTML_FLAGS += -D short_toc

IN_ITELY_FILES = $(call src-wildcard,*-intro.itely)
GENERATED_ITELY_FILES = $(IN_ITELY_FILES:%-intro.itely=$(outdir)/%.itely)

$(outdir)/%.itely: %-intro.itely %.snippet-list
	xargs $(LYS_TO_TELY) -f doctitle,texidoc,verbatim --name=$@ --template=$< < $(filter %.snippet-list, $^)

$(outdir)/lilypond-snippets.texi: $(GENERATED_ITELY_FILES) $(LY_FILES)

source-links = $(outdir)/source $(outdir)/lilypond-snippets/source

$(outdir)/source:
	@rm -f $(@)
	ln -sf $(depth) $(@)

$(outdir)/%/source:
	@rm -f $(@)
	mkdir -p $(dir $@)
	ln -sf $(depth)/.. $(@)

info: $(INFO_FILES)

ifneq ($(out),www)
# cancel install-info target and $(INFO_FILES) 'default' target
# (there is no lilypond-snippets.info without images)
local-install-info:

$(INFO_FILES):
	true
endif

local-clean:
	rm -f $(INFO_IMAGES_DIR)

ifeq ($(out),www)
# All web targets, except info image symlinks and info docs are
# installed in non-recursing target from TOP-SRC-DIR
local-install-WWW: local-install-info
local-uninstall-WWW: local-uninstall-info

local-WWW-1: info $(outdir)/lilypond-snippets.pdf \
  $(outdir)/lilypond-snippets.texi

local-WWW-2: $(outdir)/lilypond-snippets/index.html \
  $(outdir)/lilypond-snippets-big-page.html \
  $(source-links)
endif

.SECONDARY:
