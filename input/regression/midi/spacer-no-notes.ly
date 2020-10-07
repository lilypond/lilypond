\version "2.21.7"

\header {
  texidoc = "This test should produce a two-measure MIDI file in the
key of G minor, but with no notes."
}

#(ly:set-option 'warning-as-error #t)

\score {
  {
    \key g \minor
    s1*2
  }
  \midi {}
}
