depth = ..

INSTALLATION_OUT_DIR=$(vimdir)/syntax
INSTALLATION_OUT_FILES=$(LILYPOND_WORDS)
# $(outdir)/lilypond-words $(outdir)/lilypond-words.el

EXTRA_DIST_FILES=$(call src-wildcard,*.vim) vimrc

LILYPOND_WORDS = $(outdir)/lilypond-words $(outdir)/lilypond-words.vim
LILYPOND_WORDS_DEPENDS =\
  $(top-src-dir)/lily/lily-lexer.cc \
  $(buildscript-dir)/lilypond-words.py \
  $(top-src-dir)/scm/markup.scm \
  $(top-src-dir)/ly/engraver-init.ly

STEPMAKE_TEMPLATES=install-out

# vimdir defined in config.make
include $(depth)/make/stepmake.make 

local-install:
	-$(INSTALL) -d $(DESTDIR)$(vimdir)/compiler
	$(INSTALL) -m 644 lilypond-compiler.vim $(DESTDIR)$(vimdir)/compiler/lilypond.vim

	-$(INSTALL) -d $(DESTDIR)$(vimdir)/ftdetect
	$(INSTALL) -m 644 lilypond-ftdetect.vim $(DESTDIR)$(vimdir)/ftdetect/lilypond.vim

	-$(INSTALL) -d $(DESTDIR)$(vimdir)/ftplugin
	$(INSTALL) -m 644 lilypond-ftplugin.vim $(DESTDIR)$(vimdir)/ftplugin/lilypond.vim

	-$(INSTALL) -d $(DESTDIR)$(vimdir)/indent
	$(INSTALL) -m 644 lilypond-indent.vim $(DESTDIR)$(vimdir)/indent/lilypond.vim

	-$(INSTALL) -d $(DESTDIR)$(vimdir)/syntax
	$(INSTALL) -m 644 lilypond-syntax.vim $(DESTDIR)$(vimdir)/syntax/lilypond.vim

local-uninstall:
	rm $(DESTDIR)$(vimdir)/compiler/lilypond.vim
	-rmdir -p $(DESTDIR)$(vimdir)/compiler 

	rm $(DESTDIR)$(vimdir)/ftdetect/lilypond.vim
	-rmdir -p $(DESTDIR)$(vimdir)/ftdetect 

	rm $(DESTDIR)$(vimdir)/ftplugin/lilypond.vim
	-rmdir -p $(DESTDIR)$(vimdir)/ftplugin 

	rm $(DESTDIR)$(vimdir)/indent/lilypond.vim
	-rmdir -p $(DESTDIR)$(vimdir)/indent 

	rm $(DESTDIR)$(vimdir)/syntax/lilypond.vim
	-rmdir -p $(DESTDIR)$(vimdir)/syntax

$(LILYPOND_WORDS): 
	cd $(top-src-dir) && $(PYTHON) buildscripts/lilypond-words.py --words --vim --dir=$(top-build-dir)/vim/$(outconfbase)

all: $(LILYPOND_WORDS)

