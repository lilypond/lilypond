##
## settings to run LilyPond


export PATH:=$(builddir)/lily/$(outconfbase):$(builddir)/buildscripts/$(outconfbase):$(PATH)

# LilyPond is often run from within $(outdir), making a relative
# PREFIX incorrect.
export LILYPONDPREFIX:=$(build_lilypond_datadir)

export PYTHONPATH:=$(topdir)/python:$(PYTHONPATH)

## arg, TEXINPUTS, TFMFONTS, MFINPUTS may still override and thus break this
export TEXMF:={$(LILYPONDPREFIX),$(shell kpsexpand \$$TEXMF)}

export MFINPUTS:=
export TEXINPUTS:=
export TFMFONTS:=


export extra_mem_top=1000000
export extra_mem_bottom=1000000
export pool_size=500000


ifdef DEB_BUILD
export PKFONTS := $(topdir)/mf/out
export MT_DESTROOT := $(topdir)/mf/out
export DVIPSMAKEPK := mktexpk --destdir $(topdir)/mf/out
endif


# guile load path?

the-script-dir=$(wildcard $(script-dir))

ifneq ($(the-script-dir),)

### some versions apparently choke on $(message)
### $(message running from source tree stepmake)

ABC2LY = $(script-dir)/abc2ly.py
CONVERT_LY = $(script-dir)/convert-ly.py
LILYPOND = $(builddir)/lily/$(outconfbase)/lilypond
LILYPOND_BOOK = $(script-dir)/lilypond-book.py
LILYPOND_BOOK_INCLUDES = -I $(pwd) -I $(outdir) -I$(input-dir) -I $(input-dir)/tricks/ -I $(input-dir)/regression/ -I $(input-dir)/test/ -I $(input-dir)/tutorial/ -I $(builddir)/mf/$(outconfbase)/  -I $(builddir)/mf/out/
LILYPOND_BOOK_FORMAT = texi
LY2DVI = $(script-dir)/ly2dvi.py
LYS_TO_TELY = $(buildscript-dir)/lys-to-tely.py


else
### some versions apparently choke on $(message)
### $(message running from installed stepmake)

ABC2LY = $(shell $(SHELL) -c 'type -p abc2ly')
CONVERT_LY = $(shell $(SHELL) -c 'type -p convert-ly')
LILYPOND = $(shell $(SHELL) -c 'type -p lilypond')
LILYPOND_BOOK = $(shell $(SHELL) -c 'type -p lilypond-book')
LILYPOND_BOOK_INCLUDES = -I. -I.. -I$(outdir)
LILYPOND_BOOK_FORMAT = texi
LY2DVI = $(shell $(SHELL) -c 'type -p ly2dvi')
LYS_TO_TELY = $(shell $(SHELL) -c 'type -p lys-to-tely')

endif

