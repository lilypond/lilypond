" LilyPond indent file
" Language:     LilyPond
" Maintainer:   Heikki Junes <hjunes@cc.hut.fi>
" Last Change:  2004 Mar 01

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetLilyPondIndent()

" Only define the function once.
if exists("*GetLilyPondIndent")
  finish
endif

function GetLilyPondIndent()
  if v:lnum == 1
    return 0
  endif

  "Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)
  let ind = indent(lnum)

  return ind
endfunction
"
"
"
