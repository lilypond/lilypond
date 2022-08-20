\version "2.23.7"

\header {
  lsrtags = "rhythms"

  texidoc = "
If the remainder of the division of the current @code{BarNumber} by the
first argument of @code{modulo-bar-number-visible} equals its second
argument print the @code{BarNumber}.

Useful to print the @code{BarNumber} at certain distances, p.e:

@itemize
@item
@code{(modulo-bar-number-visible 3 2)} -> prints 2,5,8
@item
@code{(modulo-bar-number-visible 4 2)} -> prints 2,6,10
@item
@code{(modulo-bar-number-visible 3 1)} -> prints 3,5,7
@item
@code{(modulo-bar-number-visible 5 2)} -> prints 2,7,12
@end itemize
"

  doctitle = "Printing bar numbers using modulo-bar-number-visible"
}


\layout {
  \context {
    \Score
    \override BarNumber.break-visibility = ##(#f #t #t)
    barNumberVisibility = #(modulo-bar-number-visible 3 2)
  }
}

\relative c' {
  c1 | d | e | f \break
  g1 | e | d | c
}
