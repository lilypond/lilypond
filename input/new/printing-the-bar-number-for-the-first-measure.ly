\version "2.12.1"

\header {
  lsrtags = "rhythms"

  texidoc = "
By default, the first bar number in a score is suppressed if it is
less than or equal to `1'.  By setting @code{barNumberVisibility}
to @code{all-bar-numbers-visible}, any bar number can be printed
for the first measure and all subsequent measures.  Note that an
empty bar line must be inserted before the first note for this to
work.

"
  doctitle = "Printing the bar number for the first measure"
}

\relative c' {
  \set Score.barNumberVisibility = #all-bar-numbers-visible
  \bar ""
  c1 d e f \break
  g1 e d c
}
