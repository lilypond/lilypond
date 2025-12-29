\version "2.25.32"

\header {
  texidoc = "The @code{\\polymetric \\time} command sets staff-specific timing
properties.  The @code{\\polymetric \\default} command unsets them.

The staff named ``Test'' should change to 8/8 time in the second measure and
revert to common time in the third.  The other staves should remain in common
time."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  \fixed c' <<
    \time 4/4
    \new Staff = "A" {
      \repeat unfold 3 {
        \repeat unfold 8 f8
      }
    }
    \new Staff = "B" \with { instrumentName = "Test" } {
      \repeat unfold 8 f8

      \context Staff \polymetric \time 8/8

      \contextPropertyCheck Staff.beamExceptions #'()
      \contextPropertyCheck Staff.beatBase #1/8
      \contextPropertyCheck Staff.beatStructure #'(3 3 2)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor 1
      \contextPropertyCheck Staff.timeSignature 8/8

      \repeat unfold 8 f8

      \context Staff \polymetric \default
      %% A user would be well-advised to add \time 4/4 here, but omitting it
      %% probably makes a more sensitive test.

      \contextPropertyCheck Staff.beamExceptions \default
      \contextPropertyCheck Staff.beatBase \default
      \contextPropertyCheck Staff.beatStructure \default
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor \default
      \contextPropertyCheck Staff.timeSignature \default

      \repeat unfold 8 f8
    }
    \new Staff = "C" {
      \repeat unfold 3 {
        \repeat unfold 8 f8
      }
    }
  >>
}
