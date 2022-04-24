\version "2.23.9"

\header {
  texidoc="When a @code{BreathingSign} is aligned as a
@code{'staff-bar}, staff lines extend through it, even if it is
accompanied by a zero-width @code{BarLine} at the end of the line.

The output should show two identical staves.  Between the notes should
appear a finalis sign (like a double bar line) overlapping with a
dotted bar line; this shows that @code{BreathingSign} and
@code{BarLine} are aligned alike.  At the end of the line should
appear a finalis sign with the staff lines extending to its right
side."
}

#(set-global-staff-size 36)

\paper {
  %% tighten up the multi-score tests
  score-system-spacing.basic-distance = #6
  score-system-spacing.minimum-distance = #6
  score-system-spacing.padding = #0
}

\layout {
  \context {
    \Score
    breathMarkType = #'chantdoublebar
    \override BreathingSign.break-align-symbol = #'staff-bar
  }

  \context {
    \Staff
    \omit Clef
    \omit TimeSignature
  }
}

\score { { f'4 \breathe \bar ";" g'4 \breathe \bar "" } }
\score { { f'4 \bar ";" \breathe g'4 \bar "" \breathe } }
