\version "2.16.0"
#(set-default-paper-size "a6")

\header {
  texidoc = "The minimal page breaker stacks as many lines on pages,
only accounting for manual page break commands."
}

\book {
  \paper { #(define page-breaking ly:minimal-breaking) }
  
  \score {
    <<
      \new Staff \repeat unfold 12 { c'4 }
      \new Staff \repeat unfold 12 { c'4 }
      \new Staff { \repeat unfold 11 { c'4 } c'_\markup \right-align "\\pageBreak" }
    >>
  }
  \pageBreak
  \score {
    <<
      \new Staff \repeat unfold 24 { e'4 }
      \new Staff \repeat unfold 24 { e'4 }
      \new Staff { \repeat unfold 23 { e'4 } e'_\markup \right-align "\\noPageBreak" }
    >>
  }
  \noPageBreak
  \score {
    <<
      \new Staff \repeat unfold 12 { g'4 }
      \new Staff \repeat unfold 12 { g'4 }
      \new Staff \repeat unfold 12 { g'4 }
    >>
  }
}
