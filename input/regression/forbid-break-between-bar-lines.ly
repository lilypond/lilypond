\version "2.25.35"

\header {
  texidoc = "@code{forbidBreakBetweenBarLines} controls whether
@code{Bar_@/engraver} forbids line breaks where there is no bar line.
The output should have a break in the middle of a measure."
}

\layout {
  \context {
    \Staff
    forbidBreakBetweenBarLines = ##f
  }
}

\*10 { c'4 4 4 4 \noBreak }
