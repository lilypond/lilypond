depth = ..

INSTALLATION_OUT_DIR=$(vimdir)/syntax
INSTALLATION_OUT_FILES=$(LILYPOND_WORDS)
# $(outdir)/lilypond-words $(outdir)/lilypond-words.el

STEPMAKE_TEMPLATES=install-out

# vimdir defined in config.make
include $(depth)/make/stepmake.make 

local-install:
	mkdir -p $(vimdir)/compiler
	$(INSTALL) lilypond-compiler.vim $(vimdir)/compiler/lilypond.vim

	mkdir -p $(vimdir)/ftdetect
	$(INSTALL) lilypond-ftdetect.vim $(vimdir)/ftdetect/lilypond.vim

	mkdir -p $(vimdir)/ftplugin
	$(INSTALL) lilypond-ftplugin.vim $(vimdir)/ftplugin/lilypond.vim

	mkdir -p $(vimdir)/indent
	$(INSTALL) lilypond-indent.vim $(vimdir)/indent/lilypond.vim

	mkdir -p $(vimdir)/syntax
	$(INSTALL) lilypond-syntax.vim $(vimdir)/syntax/lilypond.vim

local-uninstall:
	rm $(vimdir)/compiler/lilypond.vim
	rmdir -p $(vimdir)/compiler 

	rm $(vimdir)/ftdetect/lilypond.vim
	rmdir -p $(vimdir)/ftdetect 

	rm $(vimdir)/ftplugin/lilypond.vim
	rmdir -p $(vimdir)/ftplugin 

	rm $(vimdir)/indent/lilypond.vim
	rmdir -p $(vimdir)/indent 

	rm $(vimdir)/syntax/lilypond.vim
	rmdir -p $(vimdir)/syntax

EXTRA_DIST_FILES=filetype.vim vimrc

LILYPOND_WORDS = $(outdir)/lilypond-words $(outdir)/lilypond-words.vim
LILYPOND_WORDS_DEPENDS =\
  $(topdir)/lily/my-lily-lexer.cc \
  $(buildscript-dir)/lilypond-words.py \
  $(topdir)/scm/new-markup.scm \
  $(topdir)/ly/engraver-init.ly

$(LILYPOND_WORDS): 
	cd $(topdir) && $(PYTHON) buildscripts/lilypond-words.py --words --vim --dir=$(builddir)/vim/$(outconfbase)

all: $(LILYPOND_WORDS)

