depth = ..

INSTALLATION_OUT_DIR=$(vimdir)/syntax
INSTALLATION_OUT_FILES=$(LILYPOND_WORDS)
# $(outdir)/lilypond-words $(outdir)/lilypond-words.el

EXTRA_DIST_FILES=$(wildcard *.vim) vimrc

LILYPOND_WORDS = $(outdir)/lilypond-words $(outdir)/lilypond-words.vim
LILYPOND_WORDS_DEPENDS =\
  $(topdir)/lily/my-lily-lexer.cc \
  $(buildscript-dir)/lilypond-words.py \
  $(topdir)/scm/new-markup.scm \
  $(topdir)/ly/engraver-init.ly

STEPMAKE_TEMPLATES=install-out

# vimdir defined in config.make
include $(depth)/make/stepmake.make 

local-install:
	mkdir -p $(vimdir)/compiler
	$(INSTALL) -m 644 lilypond-compiler.vim $(vimdir)/compiler/lilypond.vim

	mkdir -p $(vimdir)/ftdetect
	$(INSTALL) -m 644 lilypond-ftdetect.vim $(vimdir)/ftdetect/lilypond.vim

	mkdir -p $(vimdir)/ftplugin
	$(INSTALL) -m 644 lilypond-ftplugin.vim $(vimdir)/ftplugin/lilypond.vim

	mkdir -p $(vimdir)/indent
	$(INSTALL) -m 644 lilypond-indent.vim $(vimdir)/indent/lilypond.vim

	mkdir -p $(vimdir)/syntax
	$(INSTALL) -m 644 lilypond-syntax.vim $(vimdir)/syntax/lilypond.vim

local-uninstall:
	rm $(vimdir)/compiler/lilypond.vim
	-rmdir -p $(vimdir)/compiler 

	rm $(vimdir)/ftdetect/lilypond.vim
	-rmdir -p $(vimdir)/ftdetect 

	rm $(vimdir)/ftplugin/lilypond.vim
	-rmdir -p $(vimdir)/ftplugin 

	rm $(vimdir)/indent/lilypond.vim
	-rmdir -p $(vimdir)/indent 

	rm $(vimdir)/syntax/lilypond.vim
	-rmdir -p $(vimdir)/syntax

$(LILYPOND_WORDS): 
	cd $(topdir) && $(PYTHON) buildscripts/lilypond-words.py --words --vim --dir=$(builddir)/vim/$(outconfbase)

all: $(LILYPOND_WORDS)

