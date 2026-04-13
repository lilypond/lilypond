\version "2.25.35"

\header {
  texidoc = "Balloons and footnotes on multi-measure rest numbers and
percent repeat counters are correctly placed."
}

\book {
  \markup \vspace #1
  \paper {
    #(set-paper-size "a7landscape")
  }
  \new Score \with {
    \consists Balloon_engraver
    countPercentRepeats = ##t
  }
  \compressMMRests {
    \footnote #'(1 . 1) "Rest during three measures" MultiMeasureRestNumber
    R1*3
    \after 1 \balloonGrobText PercentRepeatCounter #'(2 . 1) "Second repeat"
    \%3 { c'1 }
  }
}
