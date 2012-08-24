\header {
  texidoc = "Accidentals are invalidated at clef changes."
}

\layout {
  ragged-right = ##t
}


\version "2.16.0"
\new Staff \relative c' {
    \key g\major
    \clef treble
    cis dis f
    \clef bass
    <c dis fis>
}

