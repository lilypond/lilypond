\version "2.21.0"

\header {
  texidoc = "Cue clefs can be printed after a bar line."
}

instrument = \relative {
  \repeat unfold 40 { c'8 }
}
\addQuote "instrQuote" \instrument

Solo = \relative {
  c'2 c |

  % Change the break-align-orders of the score so that cue-clef comes after bar-line
  \override Score.BarLine.space-alist.cue-clef = #'(minimum-space . 1.0)
  \override Score.BreakAlignment.break-align-orders  =
    ##(( ;; end-of-line:
         left-edge cue-end-clef ambitus breathing-sign clef staff-bar
         key-cancellation key-signature time-signature cue-clef custos)
       ( ;; unbroken
         left-edge cue-end-clef ambitus breathing-sign clef staff-bar
         key-cancellation key-signature time-signature cue-clef custos)
       ( ;; begin of line
         left-edge ambitus breathing-sign clef key-cancellation
         key-signature staff-bar time-signature cue-clef custos))

  \cueDuringWithClef "instrQuote" #UP "bass" { R1 }
  c2 c2 |

  % Revert back to default
  \revert Score.BarLine.space-alist.cue-clef
  \revert Score.BreakAlignment.break-align-orders
  \cueDuringWithClef "instrQuote" #UP "bass" { R1 }
  c2 c2 |
}

\score {
  <<
    \new Staff \Solo
  >>
}
