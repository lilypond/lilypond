:version 6.1
set autoindent
set shiftwidth=2
"
" some handy key mappings
"
" <F4>  save & make and play midi
map <F4> :w<Return>:se makeprg=ly2dvi\ -m\ %<<Return>:make<Return>:!timidity %<.midi<Return>
"
" <F5>  save & make dvi, midi, ps
map <F5> :w<Return>:se makeprg=ly2dvi\ -P\ %<<Return>:make<Return>
"
" <F6>  view ps
map <F6> :!gv -watch %<.ps &<Return>
"
" <S-F6>  view dvi
map <S-F6> :!xdvi %<.dvi &<Return>
"
" <F7>  prev error
map <F7> :cp<Return>
"
" <F8>  next error
map <F8> :cn<Return>
"
" <F9>  make
map <F9> :w<Return>:se makeprg=make\ -k<Return>:make<Return>
"
" <F10> run buffer through lily
map <F10> :w<Return>:se makeprg=lilypond\ %:t<Return>:make<Return>
"
" <S-F10> run buffer through lily -M
map <S-F10> :w<Return>:se makeprg=lilypond\ -M\ %:t<Return>:make<Return>
"
" <F12> comment region
map <F12> :g!/%.*/normal 0i%<Return>
"
" <S-F12> remove comments in region
map <S-F12> :g/%.*/normal 0x<Return>
"
" Completions in Insert/Replace-mode with <Ctrl-N>
set dictionary-=~/.vim/lilypond.words.el dictionary+=~/.vim/lilypond.words.el
set complete-=k complete+=k
" errorformat for lily (with columns) and gcc
" (how to see multiple-line error messages?)
"
se errorformat=%f:%l:%c:\ %m,%f:%l:\ %m,In\ file\ included\ from\ %f:%l:,\^I\^Ifrom\ %f:%l%m

