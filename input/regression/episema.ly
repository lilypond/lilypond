\version "2.25.5"

\header {
  texidoc = "An episema can be typeset over a single neume or a
melisma.  Its position is quantized between staff lines."
}

#(set-global-staff-size 26)

\new VaticanaScore
  \new VaticanaVoice {
    \revert Score.SpacingSpanner.packed-spacing
    a\episemInitium\episemFinis
    \[ a\episemInitium \pes b \flexa a\episemFinis \]
    \[ a\episemInitium \pes b \flexa a b\episemFinis \flexa a \]
  }
