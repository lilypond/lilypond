\version "2.25.32"

\header {
  texidoc = "In this test, the Score time signature is 3/2.  In the staves with
scaled meters, the automatic beamer should use the default exceptions for the
written time signatures."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
}

\fixed c' <<
  \new Staff {
    \time 3/2
    \repeat unfold 15 c2
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 2/2
      \contextPropertyCheck Staff.beamExceptions
      #'((end . ((1/32 . (8 8 8 8)))))
      \contextPropertyCheck Staff.beatBase #1/2
      \contextPropertyCheck Staff.beatStructure #'(1 1)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor #3/2
      \contextPropertyCheck Staff.submeasureStructure #'(2)
      \contextPropertyCheck Staff.timeSignature 2/2

      \repeat unfold 8 c8 |
      \repeat unfold 4 \tuplet 3/2 { c8 c c } |
      \repeat unfold 16 c16 |
      \repeat unfold 8 \tuplet 3/2 { c16 c c } |
      \repeat unfold 32 c32 |
    }
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 4/4
      \contextPropertyCheck Staff.beamExceptions
      #'((end . ((1/8 . (4 4))
                 (1/12 . (3 3 3 3)))))
      \contextPropertyCheck Staff.beatBase #1/4
      \contextPropertyCheck Staff.beatStructure #'(1 1 1 1)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor #3/2
      \contextPropertyCheck Staff.submeasureStructure #'(4)
      \contextPropertyCheck Staff.timeSignature 4/4

      \repeat unfold 8 c8 |
      \repeat unfold 4 \tuplet 3/2 { c8 c8 c8 } |
      \repeat unfold 16 c16 |
      \repeat unfold 8 \tuplet 3/2 { c16 c c } |
      \repeat unfold 32 c32 |
    }
  }
>>
