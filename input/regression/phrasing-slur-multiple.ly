\version "2.16.0"

#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (_ "already have phrasing slur"))
#(ly:expect-warning (_ "cannot end phrasing slur"))
#(ly:expect-warning (_ "unterminated phrasing slur"))

\header {
  texidoc = "LilyPond does not support multiple concurrent phrasing slurs with the 
parentheses syntax.  In this case, warnings will be given and the nested
slur will not be generated.  However, one can can create a second slur with
a different spanner-id."
}

sp=#(define-event-function (parser location n e) (index? ly:event?)
     (set! (ly:music-property e 'spanner-id) (format "sp~a" n))
     e)

\relative c'' { 
  % This will give warnings ("Already have phrasing slur" and "Cannot end phrasing slur")
  c4\(\(\sp1\( d4\)\(\sp1\( e4\) f\) |
  % This will give two overlapping slurs and "unterminated phrasing slur" from above
  d\(  d\sp2\( e\) f\sp2\) |
  
}
