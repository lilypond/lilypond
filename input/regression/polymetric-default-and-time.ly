\version "2.25.32"

\header {
  texidoc = "The @code{\\polymetric \\default} command works as usual when it
coincides with a @code{\\time} command that changes the measure length.  In
measure@tie{}3, both staves should change to 3/4 time."
}

#(ly:set-option 'warning-as-error #t)

\fixed c' <<
  \new Staff {
    \repeat unfold 8 c8 |
    \repeat unfold 8 c8 | % other staff is in 8/8 in this measure
    \time 3/4
    \repeat unfold 6 c8 |
  }
  \new Staff {
    \repeat unfold 8 c8 |
    \context Staff \polymetric \time 8/8
    \repeat unfold 8 c8 |
    \context Staff \polymetric \default
    %% A user would be well-advised to add \time 3/4 here, but omitting it
    %% probably makes a more sensitive test.
    \contextPropertyCheck Staff.beamExceptions \default
    \contextPropertyCheck Staff.beatBase \default
    \contextPropertyCheck Staff.beatStructure \default
    \contextPropertyCheck Staff.measureLength \default
    \contextPropertyCheck Staff.meterScalingFactor \default
    \contextPropertyCheck Staff.timeSignature \default
    \repeat unfold 6 c8 |
  }
>>
