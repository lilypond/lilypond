\header {
  texidoc = "Accidentals are invalidated at clef changes."
}

\layout {
  ragged-right = ##t
}


\version "2.19.21"
\new Staff \relative {
  \key g \major
  \clef treble
  cis' dis f
  \clef bass
  <c dis fis>
}

