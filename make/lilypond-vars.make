
export PATH:=$(abs-builddir)/lily/$(outconfbase):$(abs-builddir)/buildscripts/$(outconfbase):$(PATH)

# LilyPond is often run from within $(outdir), making a relative
# PREFIX incorrect.
export LILYPONDPREFIX:=$(shell cd $(depth)/$(builddir)/share/lilypond/$(TOPLEVEL_VERSION); pwd)
export PYTHONPATH:=$(topdir)/$(outdir):$(PYTHONPATH)


lilypondprefix = $(abs-builddir)/share/lilypond/$(TOPLEVEL_VERSION)

export TEXCONFIG:=$(abs-builddir)/mf/$(outconfbase):$(TEXCONFIG)
export TEXPSHEADERS:=$(abs-builddir)/mf/$(outconfbase)::$(TEXPSHEADERS)
export MFINPUTS:=$(abs-srcdir)/mf/:$(MFINPUTS)::
export TEXINPUTS:=$(abs-builddir)/mf/$(outconfbase)/:$(abs-srcdir)/tex/:$(abs-srcdir)/ps/:$(TEXINPUTS):$(pwd)::
export TFMFONTS:=$(abs-builddir)/mf/$(outconfbase):
# most taken care of by LILYPONDPREFIX and builddir-setup
#export LILYINCLUDE:=$(abs-srcdir)/ps:$(abs-srcdir)/scm:$(abs-srcdir)/ly:$(abs-builddir)/mf/$(outconfbase)::$(TEX_TFMDIR):$(LILYINCLUDE)
export LILYINCLUDE:=::$(TEX_TFMDIR):$(LILYINCLUDE)
export extra_mem_top=1000000
export extra_mem_bottom=1000000
export pool_size=300000


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
LILYPOND = $(abs-builddir)/lily/$(outconfbase)/lilypond
LILYPOND_BOOK = $(script-dir)/lilypond-book.py
LILYPOND_BOOK_INCLUDES = -I $(pwd) -I $(outdir) -I$(input-dir) -I $(input-dir)/tricks/ -I $(input-dir)/regression/ -I $(input-dir)/test/ -I $(input-dir)/tutorial/ -I $(abs-builddir)/mf/$(outconfbase)/  -I $(abs-builddir)/mf/out/
LY2DVI = $(script-dir)/ly2dvi.py
PS_TO_GIFS = $(buildscript-dir)/ps-to-gifs.sh
PS_TO_PNGS = $(buildscript-dir)/ps-to-pngs.sh

else
### some versions apparently choke on $(message)
### $(message running from installed stepmake)

ABC2LY = $(shell $(SHELL) -c 'type -p abc2ly')
CONVERT_LY = $(shell $(SHELL) -c 'type -p convert-ly')
LILYPOND = $(shell $(SHELL) -c 'type -p lilypond')
LILYPOND_BOOK = $(shell $(SHELL) -c 'type -p lilypond-book')
LILYPOND_BOOK_INCLUDES = -I. -I.. -I$(outdir)
LY2DVI = $(shell $(SHELL) -c 'type -p ly2dvi')
PS_TO_GIFS = $(shell $(SHELL) -c 'type -p ps-to-gifs')
PS_TO_PNGS = $(shell $(SHELL) -c 'type -p ps-to-pngs')

endif

