\version "2.21.0"

\header {
  texidoc = "@code{\\autoChange} needs to be given pitches in their
final octaves, so if @code{\\relative} is used it must be applied
inside @code{\\autoChange}.  The pitches in @code{\\autoChange} are
unaffected by an outer @code{\\relative}, so that the printed output
shows the pitches that @code{\\autoChange} used.

The expected output of this test is three identical measures."
}

\new PianoStaff {
   \autoChange \absolute {g4 c g' c}
   \relative \autoChange {g4 c g' c}  % relative is ignored
   \autoChange \relative {g4 c, g'' c,,}
}
