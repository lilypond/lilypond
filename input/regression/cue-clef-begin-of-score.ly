\version "2.21.0"

\header {
  texidoc = "Clefs for cue notes at the start of a score should print the
standard clef plus a small cue clef after the time/@/key signature."
}

vI = \relative { \clef "treble" \repeat unfold 40 g'4 }
\addQuote vIQuote { \vI }

Solo = \relative c'' {
  \clef "bass"
  \cueDuringWithClef "vIQuote" #DOWN "treble" { r2 } d,,4 d4 |
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
