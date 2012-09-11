\version "2.16.0"

\header {
  texidoc = "Unfolding tremolo repeats.  All fragments fill one
measure with 16th notes exactly."
}

\layout { ragged-right = ##t }

\relative c' {
  \time 2/4 
  \unfoldRepeats {
    \repeat tremolo 4 { c16 e } |
    \repeat tremolo 8 c16
  } |
  \time 3/4 
  \unfoldRepeats {
    \repeat tremolo 6 { c16 e } |
    \repeat tremolo 12 { c16 } |
  }
  \bar "|."  
}
