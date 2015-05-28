\header {

  texidoc = "Stem directions influence positioning of whole note
  tremolo beams."

}

\version "2.19.21"
\paper{
  ragged-right = ##t
}

\relative {
  \stemDown
  \repeat tremolo 16 { d32 a'32 }
}
