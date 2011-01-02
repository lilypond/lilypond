\version "2.13.45"

\header {
  texidoc = "Octavation for clefs for cue notes."
}

vI = \relative c'' { \clef "treble" \repeat unfold 40 g4 }
\addQuote vIQuote { \vI }

Solo = \relative c' {
  \clef "treble_8" c1 |
  \cueDuringWithClef #"vIQuote" #UP #"bass^8" { R1 } |
  c1 | \break
  c c
  \clef "bass^8" c1 |
  \cueDuringWithClef #"vIQuote" #UP #"treble_8" { R1 R1 } |
  c
  \cueDuringWithClef #"vIQuote" #UP #"treble_8" { R1 \break R } |
  c
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
