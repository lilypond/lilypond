\version "2.14.2"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "LilyPond does not support multiple concurrent slurs with the
parentheses syntax.  In this case, warnings will be given and the nested
slur will not be generated.  However, one can can create a second slur with
a different spanner-id."
}

altSlur = #(make-music 'SlurEvent 'span-direction START 'spanner-id "alt")
altSlurEnd = #(make-music 'SlurEvent 'span-direction STOP 'spanner-id "alt")

\relative c'' {
  % This will give warnings ("Already have slur" and "Cannot end slur")
  c4(( d4)( e4) f) |
  % This will give two overlapping slurs:
  d(  d\altSlur e) f\altSlurEnd |

}
