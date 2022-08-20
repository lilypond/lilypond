\version "2.23.7"

\header {
  lsrtags = "rhythms"

  texidoc = "
By default, the first bar number in a score is
suppressed if it is less than or equal to@tie{}1.  By setting
@code{barNumberVisibility} to @code{all-bar-numbers-visible}, any bar
number can be printed for the first measure and all subsequent
measures.
"

  doctitle = "Printing the bar number for the first measure"
}


\layout {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \set Score.barNumberVisibility = #all-bar-numbers-visible
  c1 | d | e | f \break
  g1 | e | d | c
}
