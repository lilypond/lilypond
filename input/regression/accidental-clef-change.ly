\header {
  texidoc = "Accidentals are reset for clef changes."
}

\layout {
  ragged-right = ##t
}


\version "2.11.51"
\new Staff \relative c' {
    \clef treble
    cis dis fis
    \clef bass
    <cis dis fis gis>
}

