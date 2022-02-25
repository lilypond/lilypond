\version "2.19.29"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "already have phrasing slur"))
#(ly:expect-warning (G_ "cannot end phrasing slur"))
#(ly:expect-warning (G_ "unterminated phrasing slur"))

\header {
  texidoc = "LilyPond does not support multiple concurrent phrasing slurs with the 
parentheses syntax.  In this case, warnings will be given and the nested
slur will not be generated.  However, one can can create a second slur with
a different spanner-id."
}

\relative { 
  % This will give warnings ("Already have phrasing slur" and "Cannot end phrasing slur")
  c''4\(\(\=1\( d4\)\(\=1\( e4\) f\) |
  % This will give two overlapping slurs and "unterminated phrasing slur" from above
  d\(  d\=2\( e\) f\=2\) |
  
}
