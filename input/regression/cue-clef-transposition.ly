\version "2.21.0"

\header {
  texidoc = "Transposition for clefs for cue notes."
}

vI = \relative { \clef "treble" \repeat unfold 40 g'4 }
\addQuote vIQuote { \vI }

Solo = \relative {
  \clef "treble_8" c'1 |
  \cueDuringWithClef "vIQuote" #UP "bass^8" { R1 } |
  c1 | \break
  c c
  \clef "bass^8" c1 |
  \cueDuringWithClef "vIQuote" #UP "treble_8" { R1 R1 } |
  c
  \cueDuringWithClef "vIQuote" #UP "treble_8" { R1 \break R } |
  c
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
