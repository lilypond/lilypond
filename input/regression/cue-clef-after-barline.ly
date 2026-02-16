\version "2.25.35"

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
  %
  \override Score.BarLine.space-alist.cue-clef = #'(minimum-space . 1.0)
  \breakAlignInsert cue-clef after staff-bar

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
