\version "2.17.7"

\header {
  texidoc = "Optional transposition for clefs for cue notes is supported
by using parentheses or brackets around the transposition number."
}

vI = \relative c'' { \clef "treble" \repeat unfold 40 g4 }
\addQuote vIQuote { \vI }

Solo = \relative c' {
  \clef "treble_8" c1 |
  \cueDuringWithClef #"vIQuote" #UP #"bass^(15)" { R1 } |
  c1 | \break
  c c
  \clef "bass^8" c1 |
  \cueDuringWithClef #"vIQuote" #UP #"G_[8]" { R1 R1 } |
  c
  \cueDuringWithClef #"vIQuote" #UP #"treble_(8)" { R1 \break R } |
  c
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
