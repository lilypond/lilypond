"
" Installed As:	~/.vim/filetype.vim
"
" my filetype file
if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
  au! BufNewFile,BufRead *.ly,*.ily		setf lilypond
  au! BufNewFile,BufRead *.itexi		setf texinfo
  " TODO: add a tely syntax file, which would basically import the texinfo
  " syntax, and then use the lilypond syntax for the embedded lilypond
  " snippets.
  "au! BufNewFile,BufRead *.tely,*.itely	setf tely
augroup END
