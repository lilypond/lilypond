:version 4.0
"
" some handy key mappings
"
" F7  prev error
map [18~ :cp
"
" F8  next error
map [19~ :cn
"
" F9  make
map [20~ :w:se makeprg=make\ -k:make
"
" F10 run buffer through lily
map [21~ :w:se makeprg=lilypond\ %:t:make
"
" F11 check buffer for quarts
map [23~ :w:se makeprg=lilypond\ -Q%:t:make
"
"
" errorformat for lily (with columns) and gcc
" (how to see multiple-line error messages?)
"
se errorformat=%f:%l:%c:\ %m,%f:%l:\ %m,In\ file\ included\ from\ %f:%l:,\^I\^Ifrom\ %f:%l%m
