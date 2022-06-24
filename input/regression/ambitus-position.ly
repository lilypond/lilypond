\version "2.23.11"

\header {
  texidoc = "Ambitus can be moved to various positions with correct
horizontal spacing in all cases."
}

\layout {
  \context {
    \Staff
    \consists Ambitus_engraver
  }
}

music = \relative {
  \key f \major
  \bar ".|:"
  f' a c f
}

\new Staff {
  \ambitusAfter key-signature
  \music
}

\new Staff {
  \ambitusAfter clef
  \music
}

\new Staff {
  \ambitusAfter time-signature
  \music
}

\new Staff {
  \ambitusAfter staff-bar
  \music
}
