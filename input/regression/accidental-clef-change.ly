\header {
  texidoc = "Accidentals are reset for clef changes."
}

\version "2.7.18"
\new Staff \relative c' {
    \clef treble
    cis dis fis
    \clef bass
    <cis dis fis gis>
}

\layout {raggedright = ##t} 
