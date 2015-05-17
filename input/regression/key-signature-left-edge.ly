\header {

  texidoc = "If the clef engraver is removed, the key signature shall use a
proper padding > 0 to the start of the staff lines."

}

\version "2.19.21"

m = \relative { \key f \major c'8 }

% Default spacing should not be affected
\score {
  \new Staff \m
}

% Key signature should not touch the left edge
\score {
  \new Staff \with {
      \remove "Clef_engraver"
  } \m
}
