\version "2.27.2"

\header {
  texidoc = "The output should have exactly two staves, labeled ``A'' and
``B''.  The one labeled ``A'' should show the@tie{}A above middle@tie{}C.  The
other should show the@tie{}B above middle@tie{}C."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  %% Move Timing to Staff to expose if \measureRemainder issues a
  %% timing-related event in Score before descending to \context Staff.
  \enablePerStaffTiming
}

%% Use a pitch transformation to expose if \measureRemainder uses its argument
%% more than once by reference.
\fixed c' <<
  {
    \measureRemainder \context Staff = "A" \with { instrumentName = "A" } a2
    \contextPropertyCheck Timing.measureLength 1
    1 |
  }
  {
    \measureRemainder { \context Staff = "B" \with { instrumentName = "B" } b2 }
    \contextPropertyCheck Timing.measureLength 1
    1 |
  }
>>
