\version "2.23.10"

\header {
  texidoc = "When there is a break without a bar line, a bar number
can be printed nevertheless.  Just like all bar numbers outside
of measure boundaries, it is hidden by default, but it can be displayed
using @code{barNumberVisibility}.  On the other hand, a bar number
resulting from a break point is not displayed if the break point
does not become a break."
}

\paper {
  line-width = 30
  indent = 0
}

{
  c'2 \break % bar number is hidden
  c'2 \noBreak
  \set Score.barNumberVisibility = #all-bar-numbers-visible
  c'2 \break % now visible at the beginning of the line
  c'2 \noBreak
  \override Score.BarNumber.break-visibility = #all-visible
  c'2 \allowBreak % now also visible at end of line
  c'2 \noBreak
  c'2 \allowBreak % mid-system bar number on \allowBreak is not visible
  c'2
}
