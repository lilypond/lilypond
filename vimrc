:version 4.0
set autoindent
set shiftwidth=2
"
" some handy key mappings
"
" F4  save & make and play midi
map <F4> :w<Return>:se makeprg=ly2dvi\ -m\ %<<Return>:make<Return>:!timidity %<.midi<Return>
"
" F5  save & make dvi, midi, ps
map <F5> :w<Return>:se makeprg=ly2dvi\ -P\ %<<Return>:make<Return>
"
" F6  view ps
map <F6> :!gv -watch %<.ps &<Return>
"
" <S-F6>  view dvi
map <S-F6> :!xdvi %<.dvi &<Return>
"
" F7  prev error
map <F7> :cp<Return>
"
" F8  next error
map <F8> :cn<Return>
"
" F9  make
map <F9> :w<Return>:se makeprg=make\ -k<Return>:make<Return>
"
" F10 run buffer through lily
map <F10> :w<Return>:se makeprg=lilypond\ %:t<Return>:make<Return>
"
" shift F10: run buffer through lily -M
map <S-F10> :w<Return>:se makeprg=lilypond\ -M\ %:t<Return>:make<Return>
"
"
" errorformat for lily (with columns) and gcc
" (how to see multiple-line error messages?)
"
se errorformat=%f:%l:%c:\ %m,%f:%l:\ %m,In\ file\ included\ from\ %f:%l:,\^I\^Ifrom\ %f:%l%m

