\version "2.15.42"

\header {

  doctitle = "beamExceptions"

  texinfo = "beamExceptions is used to modify the automatic beaming for certain durations;
  the expected grouping is given after the note duration"

}

\relative c' {
  \time 2/4
  \set Score.beamExceptions = #'(
    ( end .
      (
        ( (1 . 32) . (4 4 4 4) )
      )
    )
  )
  \repeat unfold 16 c32
  \time 3/4
  \set Score.beamExceptions = #'(
    ( end .
      (
        ( (1 . 32) . (4 4 4 4 4 4) )
      )
    )
  )
  \repeat unfold 24 c32
  c8 c32 c32 c32 c32 c16 c16 c32 c32 c32 c32 c16 c32 c32 c32 c32 c32 c32
  \time 4/4
  \set Score.beamExceptions = #'(
    ( end .
      (
        ( (1 . 32) . (4 4 4 4 4 4 4 4) )
      )
    )
  )
  \repeat unfold 32 c32
  \time 6/8
  \set Score.beamExceptions = #'(
    ( end .
      (
        ( (1 . 32) . (4 4 4 4 4 4) )
      )
    )
  )
  \repeat unfold 24 c32
}
