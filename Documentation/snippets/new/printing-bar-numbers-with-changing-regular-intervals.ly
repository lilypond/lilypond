\version "2.25.35"

\header {
  categories = "Rhythms"

  texidoc = "
Using the @code{set-bar-number-visibility} context function, bar number
intervals can be changed.
"

  doctitle = "Printing bar numbers with changing regular intervals"
} % begin verbatim


\relative c' {
  \override Score.BarNumber.break-visibility = #end-of-line-invisible
  \context Score \applyContext #(set-bar-number-visibility 4)
  \*10 c'1
  \context Score \applyContext #(set-bar-number-visibility 2)
  \*10 c
}
