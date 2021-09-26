\version "2.23.4"

\header {
  texidoc="This case places @code{\\alternative} within the body of a
@code{\\repeat volta}, with the alternatives at the end of the
repeated section.  The alternatives receive volta brackets, bar
numbers, and ending repeat bar lines.  They unfold as expected."
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
  s1_"A" \repeat volta 2 { s1_"B" \alternative { s_"C" s_"D" } }
}

\new Score \piece
\new Score \unfoldRepeats \piece
