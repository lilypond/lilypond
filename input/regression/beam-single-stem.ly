\header {

  texidoc = "Single stem beams are also allowed. For such beams,
  clip-edges is switched off automatically."

}
\version "2.19.21"

\relative {
  d'16[]
  \set stemRightBeamCount = #0
  d32[]
}

\layout {
  ragged-right = ##t
}
