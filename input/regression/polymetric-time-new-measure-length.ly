\version "2.25.32"

\header {
  texidoc = "The @code{\\polymetric \\time} command works as usual when it
coincides with a @code{\\time} command that changes the measure length.  In the
second measure, staff ``A'' should change to 3/4 time and staff ``B'' should
change to 6/8 time."
}

#(ly:set-option 'warning-as-error #t)

\fixed c' <<
  \new Staff \with { instrumentName = "A" } {
    \repeat unfold 8 c8 | % other staff is in 8/8 in this measure
    \time 3/4
    \repeat unfold 6 c8 |
  }
  \new Staff \with { instrumentName = "B" } {
    \context Staff \polymetric \time 8/8
    \repeat unfold 8 c8 |
    \context Staff \polymetric \time 6/8
    \repeat unfold 6 c8 |
  }
>>
