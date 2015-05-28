\version "2.19.21"

\header {
  texidoc = "A tremolo repeat containing only one note (no sequential music)
  shall not be scaled. An articulation or dynamic sign on the note should not
  confuse lilypond."
}

\paper { ragged-right = ##t }
\score {
  \new Staff \relative {
    \repeat tremolo 4 { a'16 }
    \repeat tremolo 4 { a16\f }
    \repeat tremolo 4 a16
    \repeat tremolo 4 a16\f |
    \repeat tremolo 4 a16\f-> 
    c2. |
  }
}
