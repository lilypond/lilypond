\version "2.4.0"
\header {
  texidoc = "
  Tremolo repeats can be constructed for short tremolos (total duration smaller than 1/4) too. Only some of the beams are connected to the stems. 
  "
}

\score {
  \context Staff  \relative c' {
    \repeat "tremolo"  2 { c32 e32 }
    \stemDown
    \repeat "tremolo"  2 { c32 e32 }
  }
  \layout { raggedright = ##t }
}

