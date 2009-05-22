"
" Installed As:	~/.vim/filetype.vim
"
" my filetype file
if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
  au! BufNewFile,BufRead *.ly,*.ily		setf lilypond
augroup END
