
\version "2.19.80"

\header {
  lsrtags = "rhythms"

  texidoc = "
Multi-measure rests have length according to their total duration which
is under the control of @code{MultiMeasureRest.space-increment}.  Note
the default value is @code{2.0}.

"
  doctitle = "Multi-measure length control"
}

\relative c' {
  \compressFullBarRests
  R1*2 R1*4 R1*64 R1*16
  \override Staff.MultiMeasureRest.space-increment = 2.5
  R1*2 R1*4 R1*64 R1*16
}
