\version "2.21.0"

\header {
  texidoc = "
Setting @code{restNumberThreshold} affects only future multi measure
rests. Unsetting it works without crashes.

The rests should be numbered 2, (none), 1, 2, 1, (none), and 2.
"
}

{
  \compressEmptyMeasures
  R1*2 R1
  \set restNumberThreshold = 0
  R1 R1*2 R1
  \unset restNumberThreshold
  R1 R1*2
}
