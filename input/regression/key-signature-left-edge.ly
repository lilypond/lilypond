\header {

  texidoc = "If the clef engraver is removed, the key signature shall use a
proper padding > 0 to the start of the staff lines."

}

\version "2.13.7"

m = \relative c' { \key f \major c8 }

% Default spacing should not be affected
\new Score  {
  \new Staff \m
}

% Key signature should not touch the left edge
\new Score  {
  \new Staff \with {
      \remove "Clef_engraver"
  } \m
}
