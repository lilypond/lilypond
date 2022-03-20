\version "2.23.7"

\header {
  texidoc = "Alignments for breakable items can have different values set
  for each break direction using the @code{break-alignment-list} function."
}

\layout {
  indent = 0
  line-width = 50
  \context {
    \Score
    currentBarNumber = #11
    barNumberVisibility = #all-bar-numbers-visible
    \override BarNumber.break-visibility = #'#(#t #t #t)
  }
}

{
  \omit Staff.TimeSignature
  \override Score.BarNumber.self-alignment-X = #(break-alignment-list RIGHT CENTER LEFT)
  c'1 c'1

  \break
  \override Score.BarNumber.self-alignment-X = #(break-alignment-list RIGHT LEFT CENTER)
  c'1 c'1

  \break
  \override Score.BarNumber.self-alignment-X = #(break-alignment-list LEFT RIGHT RIGHT)
  c'1 c'1
  \override Score.BarNumber.self-alignment-X = #(break-alignment-list CENTER CENTER CENTER)
}
