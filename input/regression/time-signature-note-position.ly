\version "2.25.30"

\header{
  texidoc="@code{TimeSignature} grob properties @code{note-@/staff-@/position}
and @code{note-@/dots-@/direction} allow adjusting the vertical position of the
note and augmentation dots."
}

#(ly:set-option 'warning-as-error #t)

{
  \override Timing.TimeSignature.style = #'numbered
  \override Timing.TimeSignature.denominator-style = #'note

  \tempo "(default)"
  \once \override Timing.TimeSignature.time-signature = #'(2 . 8/3)
  \time 6/8
  a'2.

  \tempo "-1"
  \override Timing.TimeSignature.note-staff-position = #-1
  \once \override Timing.TimeSignature.time-signature = #'(2 . 8/3)
  \time 6/8
  a'2.

  \tempo "-2, UP"
  \override Timing.TimeSignature.note-staff-position = #-2
  \override Timing.TimeSignature.note-dots-direction = #UP
  \once \override Timing.TimeSignature.time-signature = #'(2 . 8/3)
  \time 6/8
  a'2.

  \tempo "-2, DOWN"
  \override Timing.TimeSignature.note-staff-position = #-2
  \override Timing.TimeSignature.note-dots-direction = #DOWN
  \once \override Timing.TimeSignature.time-signature = #'(2 . 8/3)
  \time 6/8
  a'2.
}
