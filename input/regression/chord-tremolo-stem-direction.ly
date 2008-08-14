\header {

  texidoc = "Stem directions influence positioning of whole note
  tremolo beams."

}

\version "2.11.51"
\paper{
  ragged-right = ##t
}

\relative {
  \stemDown
  \repeat tremolo 16 {d,32 a'32 }
} 
