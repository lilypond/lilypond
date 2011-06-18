\header {
  texidoc = "Multi-measure rests standard values can be tweaked."
}

\version "2.15.0"
\new Staff {
  \override MultiMeasureRest #'usable-duration-logs = #'(2 1)
  \time 1/4 R4-"Use non-standard multi-measure rests."
  \time 2/4 R2
}
\new Staff {
  \override MultiMeasureRest #'round-to-longer-rest = ##t
  \time 3/2 R1.-"Round to the longer rest." \time 7/2 R\breve..
}
