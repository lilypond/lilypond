\version "2.25.33"

\header {
  texidoc = "@code{\approximatePitch \foo} does not affect other references to
@code{\foo}.  Only the middle note should have a triangular head."
}

#(ly:set-option 'warning-as-error #t)

testNote = c'4

{
  \testNote
  \approximatePitch \testNote
  \testNote
}
