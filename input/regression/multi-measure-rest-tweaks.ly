\header {
  texidoc = "Multi-measure rests standard values can be tweaked."
}

\version "2.17.6"

\markup "Use non-standard multi-measure rests:"
\new Staff {
  \override MultiMeasureRest.usable-duration-logs = #(iota 2 1)
  \time 1/4 R4
  \time 2/4 R2
}
\markup "Round up to the longer rest:"
\new Staff {
  \override MultiMeasureRest.round-up-to-longer-rest = ##t
  \time 3/2 R1.
  \time 7/2 R\breve..
}
\markup "Round up to the longer rest only in specified time signatures:"
\new Staff {
  \override MultiMeasureRest.round-up-exceptions = #'((3 . 2))
  \time 3/2 R1.
  \time 7/2 R\breve..
  \time 3/2 R1.
}
