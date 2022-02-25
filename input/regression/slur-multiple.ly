\version "2.19.29"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "already have slur"))
#(ly:expect-warning (G_ "cannot end slur"))
#(ly:expect-warning (G_ "unterminated slur"))

\header {
  texidoc = "LilyPond does not support multiple concurrent slurs with the 
parentheses syntax.  In this case, warnings will be given and the nested
slur will not be generated.  However, one can can create a second slur with
a different spanner-id."
}

\relative { 
  % This will give warnings ("Already have slur" and "Cannot end slur")
  c''4((\=1( d4)(\=1( e4) f) |
  % This will give two overlapping slurs and "unterminated slur" from above
  d(  d\=2( e) f\=2) |
  
}
