\version "2.25.32"

\header {
  texidoc="Submeasure bar lines are placed before any grace notes in the
submeasure."
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

\new Score \with {
  submeasureBarType = "!"
  measureBarType = #'()
  \time #'((3 . 4) (3 . 8))
} <<
  \new Staff \fixed c' {
    \repeat unfold 2 {
      \appoggiatura d8
      \repeat unfold 3 c4
      \appoggiatura d8
      \repeat unfold 3 { c8 }
    }
  }
  \new Staff \fixed c' {
    \repeat unfold 2 {
      \grace { e8 d8 }
      c2.
      \grace { e8 d8 }
      c4.
    }
  }
>>
