#
## settings to run LilyPond
ifeq ($(LILYPOND_EXTERNAL_BINARY),)

# environment settings.
export PATH:=$(top-build-dir)/lily/$(outconfbase):$(top-build-dir)/scripts/$(outconfbase):$(PATH)
export LILYPOND_BINARY=$(top-build-dir)/$(outconfbase)/bin/lilypond
else

## better not take the binaries  from a precompiled bundle, as they
## rely on env vars for relocation.
##

#export PATH:=$(dir $(LILYPOND_EXTERNAL_BINARY)):$(PATH)
export LILYPOND_BINARY=$(LILYPOND_EXTERNAL_BINARY)
endif

# Don't create __pycache__ in the source directory.
export PYTHONDONTWRITEBYTECODE=1
export PYTHONPATH:=$(top-src-dir)/python:$(PYTHONPATH)

ABC2LY = $(script-dir)/abc2ly.py
MIDI2LY = $(script-dir)/midi2ly.py
MUSICXML2LY = $(script-dir)/musicxml2ly.py
CONVERT_LY = $(script-dir)/convert-ly.py
LILYPOND_BOOK = $(script-dir)/lilypond-book.py

## override from cmd line to speed up.
ANTI_ALIAS_FACTOR=2
LILYPOND_JOBS=$(if $(CPU_COUNT),-djob-count=$(CPU_COUNT),)
LANG_TEXIDOC_FLAGS:=$(foreach lang,$(LANGS),--header=texidoc$(lang))
LANG_DOCTITLE_FLAGS:=$(foreach lang,$(LANGS),--header=doctitle$(lang))

LILYPOND_BOOK_LILYPOND_FLAGS=$(LOCAL_LILYPOND_FLAGS) \
-dseparate-page-formats=ps,pdf \
$(LILYPOND_JOBS) \
-dinclude-eps-fonts \
-dgs-load-fonts \
--header=doctitle \
$(LANG_DOCTITLE_FLAGS) \
--header=texidoc \
$(LANG_TEXIDOC_FLAGS) \
-dcheck-internal-types \
-danti-alias-factor=$(ANTI_ALIAS_FACTOR)

ifeq ($(USE_EXTRACTPDFMARK),yes)
LILYPOND_BOOK_LILYPOND_FLAGS+= \
-dfont-ps-resdir=$(top-build-dir)/out-fonts -O TeX-GS
endif

ifdef SILENT
  LILYPOND_BOOK_WARN = --loglevel=WARN
else
 ifdef VERBOSE
  LILYPOND_BOOK_WARN = --loglevel=PROGRESS
 else
  LILYPOND_BOOK_WARN = --loglevel=BASIC
 endif
endif

LILYPOND_BOOK_INFO_IMAGES_DIR = $(if $(INFO_IMAGES_DIR),--info-images-dir=$(INFO_IMAGES_DIR),)
LILYPOND_BOOK_FLAGS = $(LILYPOND_BOOK_WARN) $(LILYPOND_BOOK_INFO_IMAGES_DIR)

ifeq ($(out),)
LILYPOND_BOOK_PROCESS = true
LILYPOND_BOOK_FLAGS += --skip-lily-check
else
LILYPOND_BOOK_PROCESS = $(LILYPOND_BINARY)
endif
ifeq ($(out),test)
LILYPOND_BOOK_FLAGS += --skip-png-check
endif

# The various flavours of TeX support environment variables to search input
# files.  We have to prepend directories (as indicated by the trailing `;`
# characters) to some of them so that our generated LM typewriter fonts in
# OT1 font encoding are found.
#
#    variable     type
#   --------------------------------------------------------
#    TEXINPUTS    TeX input source files
#    TFMFONTS     TeX metric files (suffix `.tfm`)
#    TEXFONTMAPS  font map files (suffix `.map`)
#    ENCFONTS     PostScript encoding files (suffix `.enc`)
#
export TEXINPUTS=$(top-src-dir)/Documentation/tex/:
export TFMFONTS=$(top-build-dir)/Documentation/tex/$(outdir);
export TEXFONTMAPS=$(top-build-dir)/Documentation/tex/$(outdir);
export ENCFONTS=$(top-build-dir)/Documentation/tex/$(outdir);

export LYDOC_LOCALEDIR:= $(top-build-dir)/Documentation/po/out

#texi-html for www only:
LILYPOND_BOOK_FORMAT=$(if $(subst out-www,,$(notdir $(outdir))),texi,texi-html)
LY2DVI = $(LILYPOND_BINARY)
LYS_TO_TELY = $(PYTHON) $(buildscript-dir)/lys-to-tely.py
