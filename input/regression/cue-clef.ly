\version "2.21.0"

\header {
  texidoc = "Clefs for cue notes: Print a cue clef at the begin of the cue
notes and a canceling clef after the cue notes."
}

vI = \relative { \clef "treble" \repeat unfold 16 g'4 }
\addQuote vIQuote { \vI }

Solo = \relative {
  \clef "bass"
  c4 \cueDuringWithClef "vIQuote" #DOWN "treble" {
    r4 r2 |
    r4
  } c4 c2 |
  \cueDuringWithClef "vIQuote" #DOWN "soprano" { R1*2 } |
  c1
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
