\version "2.23.6"

\header {
  texidoc="This case places @code{\\alternative} within the body of a
@code{\\repeat segno}, with the alternatives at the end of the
repeated section, but with volta numbers out of order.  Alternative
bar numbering is enabled.

The alternatives are notated with brackets rather than coda signs.
Repetition is notated with a segno and simplified @emph{D.S.}
instructions that have no return counts or section labels.
Alternative bar numbers appear."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    alternativeNumberingStyle = #'numbers-with-letters
    \override BarNumber.break-visibility = #all-visible
  }
}

piece = \fixed c' {
  s1_"A"
  \repeat segno 4 {
    s1_"B"
    \alternative {
      \volta 1,3 s_"C"
      \volta 2,4 s_"D"
    }
  }
  \sectionLabel "Coda"
  s_"E"
}

\new Score \piece
\new Score \unfoldRepeats \piece
