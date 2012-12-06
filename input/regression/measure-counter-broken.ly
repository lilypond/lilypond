\version "2.17.6"

\header {
  texidoc = "Measures split across line breaks may be numbered in a measure
count.  Each segment receives a number.  The first number has its ordinary
appearance, but numbers after the break are enclosed in parentheses."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \startMeasureCount
  a4 b c d
  a4 b
  \bar ""
  \break
  c4 d
  a4 b c d
  \stopMeasureCount
}

\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
  }
}
