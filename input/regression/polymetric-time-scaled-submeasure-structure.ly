\version "2.25.32"

\header {
  texidoc = "In this test, the @code{Score} time signature is 3/2.  In the
staves with alternating meters, dotted bar lines should correspond to the plus
signs in the time signatures."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  \context {
    \Score
    \submeasureBarsOn
  }
}

\fixed c' <<
  \new Staff {
    \time 3/2
    c2 c2 c2
  }
  \new Staff {
    \scaleDurations 6/5 {
      \context Staff \polymetric \time #'((1 . 2) (3 . 4))
      \contextPropertyCheck Staff.beatBase #1/4
      \contextPropertyCheck Staff.beatStructure #'(2  1 1 1)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor #6/5
      \contextPropertyCheck Staff.submeasureStructure #'(2 3)
      \contextPropertyCheck Staff.timeSignature #'((1 . 2) (3 . 4))

      \repeat unfold 10 c8
    }
  }
  \new Staff {
    \scaleDurations 24/15 {
      \context Staff \polymetric \time #'((3 . 16) (2 . 8) (2 . 4))
      \contextPropertyCheck Staff.beatBase #1/16
      \contextPropertyCheck Staff.beatStructure #'(1 1 1  2 2  4 4)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor #24/15
      \contextPropertyCheck Staff.submeasureStructure #'(3 4 8)
      \contextPropertyCheck Staff.timeSignature #'((3 . 16) (2 . 8) (2 . 4))

      \repeat unfold 15 c16
    }
  }
>>
