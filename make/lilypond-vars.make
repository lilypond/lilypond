##
## settings to run LilyPond
ifeq ($(LILYPOND_EXTERNAL_BINARY),)

# environment settings.
export PATH:=$(top-build-dir)/lily/$(outconfbase):$(buildscript-dir):$(top-build-dir)/scripts/$(outconfbase):$(PATH):
export LILYPOND_BINARY=$(top-build-dir)/$(outconfbase)/bin/lilypond
else

## better not take the binaries  from a precompiled bundle, as they
## rely on env vars for relocation.
##

#export PATH:=$(dir $(LILYPOND_EXTERNAL_BINARY)):$(PATH)
export LILYPOND_BINARY=$(LILYPOND_EXTERNAL_BINARY)
endif

LANGS=$(shell $(PYTHON) $(top-src-dir)/python/langdefs.py)

export PYTHONPATH:=$(top-build-dir)/python/$(outconfbase):$(PYTHONPATH)

the-script-dir=$(wildcard $(script-dir))

ABC2LY = $(script-dir)/abc2ly.py
MUSICXML2LY = $(script-dir)/musicxml2ly.py
CONVERT_LY = $(script-dir)/convert-ly.py
LILYPOND_BOOK = $(script-dir)/lilypond-book.py

LILYPOND_BOOK_INCLUDES = -I $(src-dir)/ -I $(outdir) -I $(input-dir)	\
 -I $(top-src-dir)/Documentation -I $(top-src-dir)/Documentation/snippets \
 -I $(input-dir)/regression/ -I $(top-src-dir)/Documentation/included/ \
 -I $(top-build-dir)/mf/$(outconfbase)/	\
 -I $(top-build-dir)/mf/out/ \
 -I $(top-src-dir)/Documentation/pictures \
 -I $(top-build-dir)/Documentation/pictures/$(outdir)

## override from cmd line to speed up. 
ANTI_ALIAS_FACTOR=2
LILYPOND_JOBS=$(if $(CPU_COUNT),-djob-count=$(CPU_COUNT),)
LANG_TEXIDOC_FLAGS:=$(foreach lang,$(LANGS),--header=texidoc$(lang))
LANG_DOCTITLE_FLAGS:=$(foreach lang,$(LANGS),--header=doctitle$(lang))

LILYPOND_BOOK_LILYPOND_FLAGS=-dbackend=eps \
--formats=ps,png,pdf \
$(LILYPOND_JOBS) \
-dinclude-eps-fonts \
-dgs-load-fonts \
--header=doctitle \
$(LANG_DOCTITLE_FLAGS) \
--header=texidoc \
$(LANG_TEXIDOC_FLAGS) \
-dcheck-internal-types \
-ddump-signatures \
-danti-alias-factor=$(ANTI_ALIAS_FACTOR)

ifdef QUIET_BUILD
LILYPOND_BOOK_VERBOSE =
else
LILYPOND_BOOK_VERBOSE = --verbose
endif

LILYPOND_BOOK_INFO_IMAGES_DIR = $(if $(INFO_IMAGES_DIR),--info-images-dir=$(INFO_IMAGES_DIR),)
LILYPOND_BOOK_FLAGS = $(LILYPOND_BOOK_VERBOSE) $(LILYPOND_BOOK_INFO_IMAGES_DIR)

ifeq ($(out),)
LILYPOND_BOOK_PROCESS = true
LILYPOND_BOOK_FLAGS += --skip-lily-check
else
LILYPOND_BOOK_PROCESS = $(LILYPOND_BINARY)
endif
ifeq ($(out),test)
LILYPOND_BOOK_FLAGS += --skip-png-check
endif

TEXINPUTS=$(top-src-dir)/tex/::
export TEXINPUTS

export LYDOC_LOCALEDIR:= $(top-build-dir)/Documentation/po/out-www

#texi-html for www only:
LILYPOND_BOOK_FORMAT=$(if $(subst out-www,,$(notdir $(outdir))),texi,texi-html)
LY2DVI = $(LILYPOND_BINARY)
LYS_TO_TELY = $(buildscript-dir)/lys-to-tely

