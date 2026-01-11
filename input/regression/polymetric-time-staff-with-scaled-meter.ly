\version "2.25.32"

\header {
  texidoc = "@code{\\scaleDurations @dots{} \\polymetric \\time @dots{}} can be
used in a @code{\with} block.  In the first measure, the bass staff should have
a time signature of (3+1)/1, and it should contain four whole notes aligned with
the four quarter notes in the treble staff.  In the second measure, the bass
staff should revert to 4/4 time."
}

#(ly:set-option 'warning-as-error #t)

\fixed c' <<
  \new Staff {
    \repeat unfold 8 c4
  }
  \new Staff \with {
    \scaleDurations 1/4 { \context Staff \polymetric \time #'((3 1) . 1) }
    \clef "bass"
  } {
    \scaleDurations 1/4 {
      \repeat unfold 4 c1
    }
    \context Staff \polymetric \default
    \contextPropertyCheck Staff.beamExceptions \default
    \contextPropertyCheck Staff.beatBase \default
    \contextPropertyCheck Staff.beatStructure \default
    \contextPropertyCheck Staff.measureLength \default
    \contextPropertyCheck Staff.meterScalingFactor \default
    \contextPropertyCheck Staff.submeasureStructure \default
    \repeat unfold 4 c4
  }
>>
