\version "2.19.23"

\header {
  texidoc = "@code{\\autochange} needs to be given pitches in their
final octaves, so if @code{\\relative} is used it must be applied
inside @code{\\autochange}.  The pitches in @code{\\autochange} are
unaffected by an outer @code{\\relative}, so that the printed output
shows the pitches that @code{\\autochange} used.

The expected output of this test is three identical measures."
}

\new PianoStaff {
   \autochange \absolute {g4 c g' c}
   \relative \autochange {g4 c g' c}  % relative is ignored
   \autochange \relative {g4 c, g'' c,,}
}
