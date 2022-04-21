\version "2.23.9"

\header {
  texidoc = "@code{self-alignment-X} can be overridden on centered bar numbers."
}

\new Score \with {
  centerBarNumbers = ##t
  currentBarNumber = 100
} {
  c'4 4 4 4
  \override Score.CenteredBarNumber.self-alignment-X = #LEFT
  4 4 4 4
  \override Score.CenteredBarNumber.self-alignment-X = #RIGHT
  4 4 4 4
}
