\version "2.15.17"

\header {
  lsrtags = "workaround, contexts-and-engravers, contemporary-notation, rhythms"

  doctitle = "Changing time signatures inside a polymetric section using \\scaleDurations"
}


\layout {
  \context {
    \Score
    \remove "Timing_translator"
    \remove "Default_bar_line_engraver"
  }
  \context {
    \Staff
    \consists "Timing_translator"
    \consists "Default_bar_line_engraver"
  }
}

<<
  \new Staff {
    \scaleDurations 8/5 {
      \time 6/8
      \set Timing.measureLength = #(ly:make-moment 6 5)
      b8 b b b b b
      \time 2/4
      \set Timing.measureLength = #(ly:make-moment 4 5)
      b4 b
    }
  }
  \new Staff {
    \clef bass
    \time 2/4
    c2 d e f
  }
>>
