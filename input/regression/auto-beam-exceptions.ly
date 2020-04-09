\version "2.19.0"

\header {

  doctitle = "beamExceptions"

  texidoc = "@code{beamExceptions} is used to modify the automatic beaming for
  certain durations; the expected grouping is given after the note duration."

}

\relative c' {
  \time 2/4
  \set Score.beamExceptions =
    \beamExceptions \repeat unfold 4 { 32[ 32 32 32] }
  \repeat unfold 16 c32
  \time 3/4
  \set Score.beamExceptions =
    \beamExceptions \repeat unfold 6 { 32[ 32 32 32] }
  \repeat unfold 24 c32
  c8 c32 c32 c32 c32 c16 c16 c32 c32 c32 c32 c16 c32 c32 c32 c32 c32 c32
  \time 4/4
  \set Score.beamExceptions =
    \beamExceptions \repeat unfold 8 { 32[ 32 32 32] }
  \repeat unfold 32 c32
  \time 6/8
  \set Score.beamExceptions =
    \beamExceptions \repeat unfold 6 { 32[ 32 32 32] }
  \repeat unfold 24 c32
}
