depth = ..

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

EXTRA_DIST_FILES=filetype.vim
