\version "2.13.45"

\header {
  texidoc = "Clefs for cue notes: Print a cue clef at the begin of the cue
notes and a cancelling clef after the cue notes."
}

vI = \relative c'' { \clef "treble" \repeat unfold 16 g4 }
\addQuote vIQuote { \vI }

Solo = \relative c {
  \clef "bass"
  c4 \cueDuringWithClef #"vIQuote" #DOWN #"treble" {
    r4 r2 |
    r4
  } c4 c2 |
  \cueDuringWithClef #"vIQuote" #DOWN "soprano" { R1*2 } |
  c1
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
