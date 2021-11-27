\version "2.23.6"

\header {
  texidoc="This case places @code{\\alternative} within the body of a
@code{\\repeat segno}, with the alternatives starting at the start of
the repeated section and ending before the end of the section.  The
alternatives receive volta brackets and bar numbers, but no coda marks
or ending repeat bar lines.  They unfold as expected."
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
  s1_"A" \repeat segno 2 { \alternative { s_"B" s_"C" } s_"D" }
}

\new Score \piece
\new Score \unfoldRepeats \piece
