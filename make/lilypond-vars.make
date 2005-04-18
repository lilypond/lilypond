##
## settings to run LilyPond

# environment settings.
export PATH:=$(builddir)/lily/$(outconfbase):$(builddir)/buildscripts/$(outconfbase):$(builddir)/scripts/$(outconfbase):$(PATH):
export LILYPONDPREFIX:=$(build_lilypond_datadir)/$(TOPLEVEL_VERSION)
export PYTHONPATH:=$(builddir)/python/$(outconfbase):$(PYTHONPATH)
export DVIPSHEADERS:=$(builddir)/mf/out::




the-script-dir=$(wildcard $(script-dir))


ABC2LY = $(script-dir)/abc2ly.py
CONVERT_LY = $(script-dir)/convert-ly.py
LILYPOND = $(builddir)/lily/$(outconfbase)/lilypond
LILYPOND_BOOK = $(script-dir)/lilypond-book.py
LILYPOND_BOOK_INCLUDES = -I $(pwd) -I $(outdir) -I$(input-dir) -I $(input-dir)/regression/ -I $(input-dir)/test/ -I $(input-dir)/tutorial/ -I $(builddir)/mf/$(outconfbase)/  -I $(builddir)/mf/out/
LILYPOND_BOOK_FLAGS = --process="lilypond --backend=eps --formats=ps,png --header=texidoc -I $(srcdir)/input/test -e '(ly:set-option (quote internal-type-checking) \#t)'"


#texi-html for www only:
LILYPOND_BOOK_FORMAT=$(if $(subst out-www,,$(notdir $(outdir))),texi,texi-html)
LY2DVI = $(LILYPOND)
LYS_TO_TELY = $(buildscript-dir)/lys-to-tely.py
