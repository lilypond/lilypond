" Vim syntax file
" Language:	LilyPond
" Maintainer:	Heikki Junes <hjunes@cc.hut.fi>
" Created:      Oct 17, 2002
" Last Change:	Oct 17, 2002
" Version:	6.1-1
" Latest:
" http://savannah.gnu.org/cgi-bin/viewcvs/lilypond/lilypond/lilypond.vim

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Match also parethesis of angle type
set mps+=<:>

syn cluster lilyMatchGroup	contains=lilyMatcher,lilyString,lilyComment,lilyStatement,lilyNote,lilyNumber,lilyEquation,lilySlur,lilySpecial

syn region lilyMatcher	matchgroup=Delimiter start="{" skip="\\\\\|\\[{<>}]"	end="}"	contains=@lilyMatchGroup fold
syn region lilyMatcher	matchgroup=Delimiter start="\["		end="]"	contains=@lilyMatchGroup fold
syn region lilyMatcher	matchgroup=Delimiter start="<" skip="\\\\\|\\[{<>}]" end=">"	contains=@lilyMatchGroup fold

syn region lilyString	start=/"/ end=/"/ skip=/\\"/
syn region lilyComment	start="%{" skip="%$" end="%}"
syn region lilyComment	start="%\([^{]\|$\)" end="$"

syn match lilyStatement	"[-_^]\?\\\a\+"
syn match lilyNote	"\<\(\(\(bb\|as[ae]s\|[ae]s\|eses\|[a-h]\([ei]s\)\{,2}\)\([,']\)\{,4}\([?!]\)\?\|[srR]\)\(\(128\|6\?4\|3\?2\|16\?\|8\|\\breve\)[.]*\)\?\)\(\A\|\n\)"me=e-1
syn match lilyNumber	"[-_^.]\?\d\+[.]\?"
syn match lilyEquation	"\(#['`]\)\?\(\a*[-]\)*\a*\s*=\s*\(#[#'`]\?\)\?\a*"
syn match lilySlur	"[(~)]"
syn match lilySlur	"\\[()]"
syn match lilySpecial	"\\[<!>\\]"

" Rest of syntax highlighting rules start here
"
" " Define the default highlighting.
" " For version 5.7 and earlier: only when not done already
" " For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lily_syn_inits")
  if version < 508
    let did_lily_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink Delimiter	Identifier
  
  HiLink lilyString	String
  HiLink lilyComment	Comment
 
  HiLink lilyNote	Identifier
  HiLink lilyNumber	Constant
  HiLink lilyStatement	Statement
  HiLink lilySpecial	Special
  HiLink lilySlur	ModeMsg

  delcommand HiLink
endif
