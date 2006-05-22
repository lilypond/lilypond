\header {

  texidoc = "Single stem beams are also allowed. For such beams,
  clip-edges is switched off automatically."

}
\version "2.9.6"

\relative c' {
  d16[]
  \set stemRightBeamCount = #0
  d32[]
}

\layout {
  ragged-right = ##t
}
