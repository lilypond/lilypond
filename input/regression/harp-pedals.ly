\version "2.12.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Basic harp diagram functionality, including circled pedal boxes. 
The third diagram uses an empty string, the third contains invalid characters. 
Both cases will create warnings, but should still not fail with an error."
}

\relative c'' {
  c1^\markup \harp-pedal #"^v-|vv-^"
  % circled boxes:
  c1^\markup \harp-pedal #"o^ovo-|vovo-o^"
  % invalid pedal specifications, which still should be handled gracefully:
  c1^\markup \harp-pedal #""
  c1^\markup \harp-pedal #"asfdvx" %\break
}
