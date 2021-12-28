" LilyPond filetype file
" Language:     LilyPond
" Maintainer:   Heikki Junes <hjunes@cc.hut.fi>
" License:      This file is part of LilyPond, the GNU music typesetter.
"
"               Copyright (C) 2004--2022 Heikki Junes <hjunes@cc.hut.fi>
"
"               LilyPond is free software: you can redistribute it and/or modify
"               it under the terms of the GNU General Public License as published by
"               the Free Software Foundation, either version 3 of the License, or
"               (at your option) any later version.
"
"               LilyPond is distributed in the hope that it will be useful,
"               but WITHOUT ANY WARRANTY; without even the implied warranty of
"               MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"               GNU General Public License for more details.
"
"               You should have received a copy of the GNU General Public License
"               along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
"
" Last Change:  2016 Nov 22
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
