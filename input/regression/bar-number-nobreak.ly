\version "2.23.10"

\header {
  texidoc = "@code{\noBreak} does not prevent bar numbers from
being printed."
}

{
  \override Score.BarNumber.break-visibility = #all-visible
  c'1 \noBreak 1
}
