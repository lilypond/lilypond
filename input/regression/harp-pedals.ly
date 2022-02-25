\version "2.21.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "Harp pedal diagram contains ~a pedals rather than the usual 7.") 0)
#(ly:expect-warning (G_ "Harp pedal diagram does not contain a divider (usually after third pedal)."))
#(ly:expect-warning (G_ "Harp pedal diagram contains ~a pedals rather than the usual 7.") 1)
#(ly:expect-warning (G_ "Harp pedal diagram does not contain a divider (usually after third pedal)."))
#(ly:expect-warning (G_ "Unhandled entry in harp-pedal: ~a" ) "a")
#(ly:expect-warning (G_ "Unhandled entry in harp-pedal: ~a" ) "s")
#(ly:expect-warning (G_ "Unhandled entry in harp-pedal: ~a" ) "d")
#(ly:expect-warning (G_ "Unhandled entry in harp-pedal: ~a" ) "f")
#(ly:expect-warning (G_ "Unhandled entry in harp-pedal: ~a" ) "x")


\header {
  texidoc = "Basic harp diagram functionality, including circled pedal boxes. 
The third diagram uses an empty string, the third contains invalid characters. 
Both cases will create warnings, but should still not fail with an error."
}

\relative {
  c''1^\markup \harp-pedal "^v-|vv-^"
  % circled boxes:
  c1^\markup \harp-pedal "o^ovo-|vovo-o^"
  % invalid pedal specifications, which still should be handled gracefully:
  c1^\markup \harp-pedal ""
  c1^\markup \harp-pedal "asfdvx" %\break
}
