\version "2.23.14"

\header {
  texidoc = "Test the stacking of measure-centered bar numbers with
other objects."
}

\layout {
  \context {
    \Score
    centerBarNumbers = ##t
  }
}

{
  c'1 1
  \tempo "Allegro ma non troppo"
  \mark \default
  \textMark "Look at the conductor!"
  \ottava #1
  c''1 1
}

\new Score \with {
  \override CenteredBarNumberLineSpanner.direction = #DOWN
}
{
  c'1 1\sostenutoOn 1 1\sostenutoOff
}