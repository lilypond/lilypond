\header {

  texidoc = " Specifying @code{inspect-quants}, will print out
  demerit scores for the given configuration. Here, there are demerits
  for slur slope going to melody slope, and the slur ending far from
  the right edge."

}

\version "2.21.0"

\score {
  \relative c' {
    \override Slur.inspect-quants = #'(-3 . -5)
    c4( d f ais)
  }
}
