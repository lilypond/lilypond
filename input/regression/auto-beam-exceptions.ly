\version "2.25.35"

\header {
  texidoc = "@code{beamExceptions} is used to modify the automatic beaming for
  certain durations; the expected grouping is given after the note duration."
}

\relative c' {
  \time 2/4
  \set Score.beamExceptions =
    \beamExceptions \*4 { 32[ 32 32 32] }
  \*16 c32
  \time 3/4
  \set Score.beamExceptions =
    \beamExceptions \*6 { 32[ 32 32 32] }
  \*24 c32
  c8 c32 c32 c32 c32 c16 c16 c32 c32 c32 c32 c16 c32 c32 c32 c32 c32 c32
  \time 4/4
  \set Score.beamExceptions =
    \beamExceptions \*8 { 32[ 32 32 32] }
  \*32 c32
  \time 6/8
  \set Score.beamExceptions =
    \beamExceptions \*6 { 32[ 32 32 32] }
  \*24 c32
}
