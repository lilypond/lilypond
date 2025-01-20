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
  \once \override Arpeggio.positions = #'(0 . 0)
  f'2\testArpeggio
  b'4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "-0.5" "-1" }
} {
  \once \override Arpeggio.positions = #'(-1 . -0.5)
  f'2\testArpeggio
  <g' a'>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "1" "0.5" }
} {
  \once \override Arpeggio.positions = #'(0.5 . 1)
  f'2\testArpeggio
  <c'' d''>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "0.5" "-0.5" }
} {
  \once \override Arpeggio.positions = #'(-0.5 . 0.5)
  f'2\testArpeggio
  <a' cis''>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "1" "-1" }
} {
  \once \override Arpeggio.positions = #'(-1 . 1)
  f'2\testArpeggio
  <g' d''>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "0.5" "-1.5" }
} {
  \once \override Arpeggio.positions = #'(-1.5 . 0.5)
  f'2\testArpeggio
  <f' c''>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "1.5" "-1.5" }
} {
  \once \override Arpeggio.positions = #'(-1.5 . 1.5)
  f'2\testArpeggio
  <f' eis''>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "2" "-2" }
} {
  \once \override Arpeggio.positions = #'(-2 . 2)
  f'2\testArpeggio
  <e' f''>4\testArpeggio
}

\new Staff \with {
  instrumentName = \markup \tiny \right-column { "3" "-3" }
} {
  \once \override Arpeggio.positions = #'(-3 . 3)
  f'2\testArpeggio
  <c' a''>4\testArpeggio
}
