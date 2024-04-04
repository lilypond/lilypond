\version "2.23.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The dashed bar line scales with different staff space, entered with
@code{layout-set-staff-size}."
}

\layout {
  #(layout-set-staff-size 33)
}

testBar = "!"
\include "bar-line-placement.ily"
