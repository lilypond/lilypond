\version "2.25.32"

\header {
  texidoc="@code{submeasureBarType} controls the type of bar line to create at
submeasure boundaries.  The bar type noted above each staff should divide each
measure between the two parts of the strictly alternating time signature.  When
measure bar lines are disabled, submeasure bars lines should become visible at
measure boundaries also."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #begin-of-line-invisible
    \submeasureBarsOn
  }
}

testMusic = \fixed c' {
  \time #'((3 . 4) (3 . 8))
  \repeat unfold 2 {
    \repeat unfold 3 c4 \repeat unfold 3 c8
  }
}

\score {
  \new Staff { \sectionLabel "default" \testMusic }
}

\new Score \with {
  submeasureBarType = "!"
} {
  \new Staff { \sectionLabel "\"!\"" \testMusic }
}

\new Score \with {
  submeasureBarType = "!"
  measureBarType = #'()
} {
  \new Staff { \sectionLabel "\"!\" w/o measure bars" \testMusic }
}

\new Score \with {
  submeasureBarType = "|"
} {
  \new Staff { \sectionLabel "\"|\"" \testMusic }
}
