\version "2.23.3"

\header {
  texidoc = "Measure counters follow alternative numbering when active.
This also works with compressed multi-measure rests."
}

\layout {
  \context {
    \Score
    \override BarNumber.break-visibility = ##(#t #t #t)
    alternativeNumberingStyle = #'numbers-with-letters
    \override MultiMeasureRestNumber.direction = #DOWN
  }
}

music =
\compressMMRests {
  \startMeasureCount
  R1*3
  \repeat volta 3 {
    R1*2
  }
  \alternative {
    { c2 2 2 2 }
    { R1*2 }
    { R1*2 }
  }
  R1*3
  \stopMeasureCount
}

\score {
  \music
  \layout {
    \context {
      \Score
      \consists Measure_counter_engraver
    }
  }
}

\score {
  \music
  \layout {
    \context {
      \Staff
      % Test that this still works with the engraver in Staff.
      \consists Measure_counter_engraver
    }
  }
}
