\header {
  texidoc = "Accidentals are reset for clef changes."
}

\layout {
  ragged-right = ##t
}


\version "2.12.0"
\new Staff \relative c' {
    \clef treble
    cis dis fis
    \clef bass
    <cis dis fis gis>
}

