\version "2.25.24"

#(set-global-staff-size 40)

\paper {
  %% close vertical spacing of scores
  score-system-spacing.basic-distance = #8
  score-system-spacing.minimum-distance = #0
  score-system-spacing.padding = #0
}

\layout {
  \context {
    \Staff
    \omit Clef
    \omit Stem
    \omit TimeSignature
  }
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "0" "0" }
} {
  \once \override #(list testGrobName 'positions) = #'(0 . 0)
  f'2\testCommand
  b'4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "-0.5" "-1" }
} {
  \once \override #(list testGrobName 'positions) = #'(-1 . -0.5)
  f'2\testCommand
  <g' a'>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "1" "0.5" }
} {
  \once \override #(list testGrobName 'positions) = #'(0.5 . 1)
  f'2\testCommand
  <c'' d''>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "0.5" "-0.5" }
} {
  \once \override #(list testGrobName 'positions) = #'(-0.5 . 0.5)
  f'2\testCommand
  <a' cis''>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "1" "-1" }
} {
  \once \override #(list testGrobName 'positions) = #'(-1 . 1)
  f'2\testCommand
  <g' d''>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "0.5" "-1.5" }
} {
  \once \override #(list testGrobName 'positions) = #'(-1.5 . 0.5)
  f'2\testCommand
  <f' c''>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "1.5" "-1.5" }
} {
  \once \override #(list testGrobName 'positions) = #'(-1.5 . 1.5)
  f'2\testCommand
  <f' eis''>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "2" "-2" }
} {
  \once \override #(list testGrobName 'positions) = #'(-2 . 2)
  f'2\testCommand
  <e' f''>4\testCommand
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "3" "-3" }
} {
  \once \override #(list testGrobName 'positions) = #'(-3 . 3)
  f'2\testCommand
  <c' a''>4\testCommand
}
