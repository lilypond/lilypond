\version "2.23.0"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\volta} can remove arbitrary music from the main
body of a repeated section.  In each staff, a rest between those
marked 1 and 2 has been removed."
}

\score {
  \context Voice \fixed c' {
    \set Staff.instrumentName = \markup \column { "remove" "tagged" }
    \removeWithTag X { r4-1 << \tag X f-"X" >> r-2 r-3 r-4 } |
  }
}

\score {
  \context Voice \fixed c' {
    \set Staff.instrumentName = \markup \column { "unfold" "simple" }
    r4-1 \repeat unfold 1 \volta 2 f4-"X" r-2 r-3 r-4 |
  }
}

\score {
  \context Voice \fixed c' {
    \set Staff.instrumentName = \markup \column { "unfold" "{ }" }
    \repeat unfold 1 { r4-1 \volta 2 f-"X" r-2 r-3 } r-4 |
  }
}

\score {
  \context Voice \fixed c' {
    \set Staff.instrumentName = \markup \column { "unfold" "{ { } }" }
    \repeat unfold 1 { r4-1 { \volta 2 f-"X" } r-2 r-3 } r-4 |
  }
}

\score {
  \context Voice \fixed c' {
    \set Staff.instrumentName = \markup \column { "unfold" "{ << >> }" }
    \repeat unfold 1 { r4-1 << \volta 2 f-"X" >> r-2 r-3 } r-4 |
  }
}
