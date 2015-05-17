\header {

  texidoc = "A harmonic note head must be centered if the base note
  is a whole note."

}


\version "2.19.21"

\paper {
  ragged-right = ##t
}

\relative {
  <e' a\harmonic>1
  <e'' a\harmonic>1
}

