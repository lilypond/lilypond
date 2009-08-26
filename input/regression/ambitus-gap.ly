\version "2.13.4"

\header {
  texidoc = "The gaps between an @code{AmbitusLine} and its
note heads are set by the @code{gap} property."
}

\layout {
  \context {
    \Voice
    \consists "Ambitus_engraver"
  }
}

\new Staff {
  \time 2/4
  \override AmbitusLine #'gap = #1
  c'4 g''
}

