
export PATH:=$(topdir)/lily/out:$(topdir)/buildscripts/out:$(PATH)

export MFINPUTS:=$(topdir)/mf/:$(MFINPUTS)::
export TEXINPUTS:=$(topdir)/mf/out/:$(topdir)/tex/:$(topdir)/ps/:$(TEXINPUTS):$(pwd)::
export LILYINCLUDE:=$(topdir)/ps:$(topdir)/scm:$(topdir)/ly:$(topdir)/mf/out::$(TEX_TFMDIR):$(LILYINCLUDE)

export LILYPONDPREFIX:=$(depth)/


the-script-dir=$(wildcard $(script-dir))

ifneq ($(the-script-dir),)

### some versions apparently choke on $(message)
### $(message running from source tree stepmake)

ABC2LY = $(script-dir)/abc2ly.py
CONVERT_LY = $(script-dir)/convert-ly.py
LY2DVI = $(script-dir)/ly2dvi.py
LILYPOND_BOOK = $(script-dir)/lilypond-book.py
LILYPOND_BOOK_INCLUDES = -I $(pwd) -I $(input-dir)/tricks/ -I $(input-dir)/regression/ -I $(input-dir)/test/
PS_TO_GIFS = $(buildscript-dir)/ps-to-gifs.sh
PS_TO_PNGS = $(buildscript-dir)/ps-to-pngs.sh

else
### some versions apparently choke on $(message)
### $(message running from installed stepmake)

ABC2LY = $(shell $(SHELL) -c 'type -p abc2ly')
LY2DVI = $(shell $(SHELL) -c 'type -p ly2dvi')
CONVERT_LY = $(shell $(SHELL) -c 'type -p convert-ly')
LILYPOND_BOOK = $(shell $(SHELL) -c 'type -p lilypond-book')
LILYPOND_BOOK_INCLUDES = -I. -I.. -I$(outdir)
PS_TO_GIFS = $(shell $(SHELL) -c 'type -p ps-to-gifs')
PS_TO_PNGS = $(shell $(SHELL) -c 'type -p ps-to-pngs')

endif

