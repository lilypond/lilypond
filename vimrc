:version 4.0
set autoindent
set shiftwidth=2
"
" some handy key mappings
"
" F4  save & make and play midi
map <F4> :w:se makeprg=ly2dvi\ -m\ %<:make:!timidity %<.midi
"
" F5  save & make dvi, midi, ps
map <F5> :w:se makeprg=ly2dvi\ -P\ %<:make
"
" F6  view ps
map <F6> :!gv -watch %<.ps &
"
" F7  prev error
map <F7> :cp
"
" F8  next error
map <F8> :cn
"
" F9  make
map <F9> :w:se makeprg=make\ -k:make
"
" F10 run buffer through lily
map <F10> :w:se makeprg=lilypond\ %:t:make
"
" shift F10: run buffer through lily -M
map <S-F10> :w:se makeprg=lilypond\ -M\ %:t:make
"
"
" errorformat for lily (with columns) and gcc
" (how to see multiple-line error messages?)
"
se errorformat=%f:%l:%c:\ %m,%f:%l:\ %m,In\ file\ included\ from\ %f:%l:,\^I\^Ifrom\ %f:%l%m

