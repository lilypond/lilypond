\version "2.27.2"

\header {
  texidoc = "@code{\\setMeasureLengthFromHere} can be used in a volta-style
repeat alternative to end a measure early, and it can be used conditionally so
that it does not end the measure early when the repeated section is unfolded.
@code{\\measure} can be used in the following alternative without interfering.

These are the expectations of the folded output:
@itemize
@item
alternative@tie{}1 begins at measure@tie{}1 and is a partial measure
beamed @code{1,2,3}
@item
alternative@tie{}2 begins at measure@tie{}2 and holds a breve note
@end itemize

The expected beaming of measure@tie{}1 after unfolding is @code{1,2,3,2}.
"
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \overrideTimeSignatureSettings 4/4 1/8 1,2,3,2 #'()
  }
}

music = \fixed c' {
  \repeat volta 2 {
    \premeasure { c8 c8 }
    \alternative {
      \volta 1 {
        \volta #'() \setMeasureLengthFromHere 8*6
        \repeat unfold 6 c8
      }
      \volta 2 \measure c\breve
    }
  }
}

\new Score { \music }
\new Score { \unfoldRepeats \music }
