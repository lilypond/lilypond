\version "2.19.21"
\header {

  texidoc = "Harmonics get angled brackets in tablature.
  Harmonics in chords should retain their proper position,
  regardless of whether or not strings are specified.
  In this example, the harmonics should always be on string 1."

}

\new TabVoice
\relative {
  <c'\2 g'\1\harmonic>4
  <c g'\1\harmonic>4
  <c\2 g'\harmonic>4
  <c g'\harmonic>4
}
