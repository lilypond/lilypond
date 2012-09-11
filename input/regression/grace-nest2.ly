\version "2.16.0"
\header {
  texidoc = "Grace code should not be confused by nested sequential music containing grace notes; practically speaking, this means that the end-bar and measure bar coincide in this example." 

}

\layout { ragged-right= ##t }

\context Voice {
  { \grace b'8 c''2 }
  \grace b'16 c''4 c''4 \bar "|."
}

