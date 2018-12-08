\version "2.21.0"

\header {
  texidoc = "Ambitus engraver should obey middleCOffset,
middleCPosition, and the staffLineLayoutFunction.

All three staves should look the same."
}

\layout {
  \context {
    \Staff
    \consists Ambitus_engraver
  }
}

\new Staff \with {
  middleCOffset = #3
} {
  \clef alto
  d d'
}

\new Staff \with {
  \clef alto
  middleCPosition = #-4
} {
  d' d''
}

\new Staff \with {
  staffLineLayoutFunction = #ly:pitch-semitones
} {
  \clef alto
  a e'
}