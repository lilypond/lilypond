\header {
  texidoc = "Only whole, breve, longa and maxima rests are used by default for multi-measure rests."
}

\version "2.16.0"

\paper {
  ragged-right = ##t
  indent = 0
}

\new Staff {
  \time 3/8 R4.
  \time 2/4 R2
  \time 2/2 R1
  \time 2/1 R\breve
  \break
  \time 4/1 R\longa
  \time 8/1 R\maxima
}
