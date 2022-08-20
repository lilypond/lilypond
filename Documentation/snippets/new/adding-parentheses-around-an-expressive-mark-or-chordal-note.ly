\version "2.23.4"

\header {
  lsrtags = "expressive-marks, workaround"

  texidoc = "
The @code{\\parenthesize} function is a special tweak that encloses
objects in parentheses.  The associated grob is @code{Parentheses}.
"

  doctitle = "Adding parentheses around an expressive mark or chordal note"
}


\relative c' {
  c2-\parenthesize ->
  \override Parentheses.padding = #0.1
  \override Parentheses.font-size = #-4
  <d \parenthesize f a>2
}
