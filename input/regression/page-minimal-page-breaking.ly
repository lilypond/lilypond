\version "2.25.35"

#(set-default-paper-size "a6")

\header {
  texidoc = "The minimal page breaker stacks as many lines on pages,
only accounting for manual page break commands."
}

\book {
  \paper { #(define page-breaking ly:minimal-breaking) }

  \score {
    <<
      \new Staff { \*12 c'4 }
      \new Staff { \*12 c'4 }
      \new Staff { \*11 { c'4 } c'_\markup \right-align "\\pageBreak" }
    >>
  }
  \pageBreak
  \score {
    <<
      \new Staff { \*24 e'4 }
      \new Staff { \*24 e'4 }
      \new Staff { \*23 e'4 e'_\markup \right-align "\\noPageBreak" }
    >>
  }
  \noPageBreak
  \score {
    <<
      \new Staff { \*12 g'4 }
      \new Staff { \*12 g'4 }
      \new Staff { \*12 g'4 }
    >>
  }
}
