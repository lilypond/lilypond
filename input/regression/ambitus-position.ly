\version "2.21.0"

\header {
  texidoc = "Ambitus can be set at various positions with correct
horizontal spacing in all cases."
}

music = \new Staff \with {
  \consists Ambitus_engraver
} \relative {
  \key f \major
  \bar ".|:"
  f' a c f
}

\new Score \with {
  \ambitusAfter key-signature
} \music

\new Score \with {
  \ambitusAfter clef
} \music

\new Score \with {
  \ambitusAfter time-signature
} \music

\new Score \with {
  \ambitusAfter staff-bar
} \music
