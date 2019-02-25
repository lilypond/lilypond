\version "2.21.0"

\header {
  texidoc = "Optional transposition for clefs for cue notes is supported
by using parentheses or brackets around the transposition number."
}

vI = \relative { \clef "treble" \repeat unfold 40 g'4 }
\addQuote vIQuote { \vI }

Solo = \relative {
  \clef "treble_8" c'1 |
  \cueDuringWithClef "vIQuote" #UP "bass^(15)" { R }
  c

  \clef "bass^8" c |
  \cueDuringWithClef "vIQuote" #UP "treble_[8]" { R } |
  c
  \cueDuringWithClef "vIQuote" #UP "treble_(8)" { R1 \break R } |
  c

  \clef "treble_[8]" c'1 |
  \cueDuringWithClef "vIQuote" #UP "bass^(15)" { R }
  c
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
