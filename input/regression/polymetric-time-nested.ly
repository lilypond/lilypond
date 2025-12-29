\version "2.25.32"

\header {
  texidoc = "The @code{\\polymetric \\time} command allows setting different
time signatures in nested contexts.

In measure@tie{}2, the staff labeled ``Test'' should change to 5/5 time and the
two staves grouped with it should change to 3/3 time.

In measure@tie{}3, the staff labeled ``Test'' should remain in 5/5 time (no new
time signature printed), but the two staves grouped with it should revert to 1/1
time.

In measure@tie{}4, the staff labeled ``Test'' should revert to 1/1 time and the
two staves grouped with it should remain in 1/1 (no new time signature printed).

The two other staves should indicate 1/1 time from the start with no change at
any point."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  \fixed c' <<
    \time 1/1
    \new Staff = "A" { R1 R1 R1 R1 }
    \new StaffGroup <<
      {
        \skip 1
        %{ \context StaffGroup %} \polymetric \time 3/3

        \contextPropertyCheck StaffGroup.measureLength \default
        \contextPropertyCheck StaffGroup.meterScalingFactor 1
        \contextPropertyCheck StaffGroup.timeSignature 3/3

        \skip 1
        %{ \context StaffGroup %} \polymetric \default

        \contextPropertyCheck StaffGroup.measureLength \default
        \contextPropertyCheck StaffGroup.meterScalingFactor \default
        \contextPropertyCheck StaffGroup.timeSignature \default
      }
      \new Staff = "B" { R1 R1 R1 R1 }
      \new Staff = "C" \with { instrumentName = "Test" } {
        R1

        \context Staff \polymetric \time 5/5

        \contextPropertyCheck Staff.measureLength \default
        \contextPropertyCheck Staff.meterScalingFactor 1
        \contextPropertyCheck Staff.timeSignature 5/5

        R1
        R1

        \context Staff \polymetric \default
        %% A user would be well-advised to add \time 4/4 here, but omitting it
        %% probably makes a more sensitive test.

        \contextPropertyCheck Staff.measureLength \default
        \contextPropertyCheck Staff.meterScalingFactor \default
        \contextPropertyCheck Staff.timeSignature \default

        R1
      }
      \new Staff = "D" { R1 R1 R1 R1 }
    >>
    \new Staff = "E" { R1 R1 R1 R1 }
  >>
}
