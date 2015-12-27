\version "2.18.0"

\header {
  lsrtags = "rhythms, text"

  texidoc = "
The bar number interval can be changed by changing the context function
@code{set-bar-number-visibility}.

"
  doctitle = "Printing bar numbers with changing, regular intervals"
}


\relative c' {
  \override Score.BarNumber.break-visibility = #end-of-line-invisible
  \context Score \applyContext #(set-bar-number-visibility 4)
  \repeat unfold 10 c'1
  \context Score \applyContext #(set-bar-number-visibility 2)
  \repeat unfold 10 c
}
