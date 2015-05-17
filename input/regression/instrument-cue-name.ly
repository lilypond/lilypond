\version "2.19.21"

\header {

  texidoc = "The @code{Voice.instrumentCueName} property generates instrument
names for cue notes. It can also be unset properly."
  }


\relative {
  c''4
  \set Voice.instrumentCueName = "In.1"
  c2.
  \set Voice.instrumentCueName = "I.2"
  c1
  \unset Voice.instrumentCueName
  c
}
