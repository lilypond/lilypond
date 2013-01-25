\header {

  texidoc = "Horizontal spacing is bounded by the current measure length.
This means that the 3/8 setting does not affect the whole rest spacing." 

}


\version "2.17.11"

\layout {
  ragged-right = ##t
}

\score {
  \new Staff \with {
    \remove "Separating_line_group_engraver"
  } {
    \relative c' {
      \override Score.SpacingSpanner.uniform-stretching = ##t
      \set Score.proportionalNotationDuration = #(ly:make-moment 4/25)
      r1
      \time 3/8 r4.
    }
  }
} 
