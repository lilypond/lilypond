\version "2.25.6"

\header {
  texidoc = "The @code{\\clef} command does not print a clef if the clef has not
changed.  Only the system-start clef should appear."
}

{
  %% to cover issue 6615, no \clef command at the beginning
  c'1
  \clef "treble"
  c'1
  \clef "treble"
  c'1
  \clef "treble"
}
