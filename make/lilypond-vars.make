##
## settings to run LilyPond


export PATH:=$(builddir)/lily/$(outconfbase):$(builddir)/buildscripts/$(outconfbase):$(PATH)

# LilyPond is often run from within $(outdir), making a relative
# PREFIX incorrect.
export LILYPONDPREFIX:=$(build_lilypond_datadir)

export PYTHONPATH:=$(builddir)/python/$(outconfbase):$(PYTHONPATH)

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


ABC2LY = $(script-dir)/abc2ly.py
CONVERT_LY = $(script-dir)/convert-ly.py
LILYPOND = $(builddir)/lily/$(outconfbase)/lilypond
LILYPOND_BOOK = $(script-dir)/lilypond-book.py
LILYPOND_BOOK_INCLUDES = -I $(pwd) -I $(outdir) -I$(input-dir) -I $(input-dir)/regression/ -I $(input-dir)/test/ -I $(input-dir)/tutorial/ -I $(builddir)/mf/$(outconfbase)/  -I $(builddir)/mf/out/
LILYPOND_BOOK_FLAGS = --process="lilypond -f tex  --tex --header=texidoc -I $(srcdir)/input/test -e '(ly:set-option (quote internal-type-checking) \#t)'"

#texi-html for www only:
LILYPOND_BOOK_FORMAT=$(if $(subst out-www,,$(notdir $(outdir))),texi,texi-html)
LY2DVI = $(LILYPOND)
LYS_TO_TELY = $(buildscript-dir)/lys-to-tely.py
