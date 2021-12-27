\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This case nests one @code{\\alternative} within another at
the tail end of a @code{\\repeat volta}.  Alternative bar numbering is
enabled.

The outer alternative receives a volta bracket and alternative bar
numbering.

The inner alternative receives volta brackets and does not interrupt
the bar numbering of the outer alternative.

The music unfolds to AB ACDE ACDF."
}

\layout {
  \context {
    \Score
    alternativeNumberingStyle = #'numbers-with-letters
    \override BarNumber.break-visibility = #all-visible
  }
}

piece = \new Voice \fixed c' {
  \repeat volta 3 {
    s1_"A"
    \alternative {
      \volta 1 {
        s1_"B"
      }
      \volta 2,3 {
        \set Score.voltaSpannerDuration = #(ly:make-moment 1)
        s1_"C"
        s1_"D"
        \alternative {
          \volta 2 s1_"E"
          \volta 3 s1_"F"
        }
      }
    }
  }
  \fine
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
