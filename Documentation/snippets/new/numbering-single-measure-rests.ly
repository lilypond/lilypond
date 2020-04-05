\version "2.21.0"

\header {
  lsrtags = "preparing-parts, rhythms"

  texidoc = "
Multi measure rests show their length by a number except for single
measures. This can be changed by setting @code{restNumberThreshold}.

"
  doctitle = "Numbering single measure rests"
}
{
  \compressEmptyMeasures
  R1 R1*10 R1*11 \bar "||"
  \set restNumberThreshold = 0
  R1 R1*10 R1*11 \bar "||"
  \set restNumberThreshold = 10
  R1 R1*10 R1*11
}
