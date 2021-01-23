\version "2.23.1"

\header {
  texidoc="This case places @code{\\alternative} within the body of a
@code{\\repeat volta}, neither at the start nor the end of the
section.  The alternatives receive volta brackets and bar numbers, but
no repeat bar lines.  They unfold as expected."
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
  s1_"A" \repeat volta 2 { s_"B" \alternative { s_"C" s_"D" } s_"E" }
}

\new Score \piece
\new Score \unfoldRepeats \piece
