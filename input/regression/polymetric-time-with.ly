\version "2.25.32"

\header {
  texidoc = "@code{\\polymetric \\time} may be used in a new @code{Staff}'s
@code{\\with} block to set staff-specific timing properties.  The
@code{\\polymetric \\default} command can later unset them.

The staff named ``Test'' should begin in 8/8 time and revert to common time in
the second measure.  The other staves should stay in common time from the
start."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  \fixed c' <<
    \new Staff {
      \repeat unfold 2 {
        \repeat unfold 8 f8
      }
    }
    \new Staff \with {
      instrumentName = "Test"
      \context Staff \polymetric \time 8/8

      \contextPropertyCheck Staff.beamExceptions #'()
      \contextPropertyCheck Staff.beatBase #1/8
      \contextPropertyCheck Staff.beatStructure #'(3 3 2)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor 1
      \contextPropertyCheck Staff.timeSignature 8/8
    } {
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
    \new Staff {
      \repeat unfold 2 {
        \repeat unfold 8 f8
      }
    }
  >>
}
