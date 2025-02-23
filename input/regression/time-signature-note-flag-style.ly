\version "2.25.25"

\header{
  texidoc="@code{TimeSignature} grob property @code{note-@/flag-@/style}
sets the flag style for number-over-note time signatures."
}

#(ly:set-option 'warning-as-error #t)

{
  \override Timing.TimeSignature.style = #'numbered
  \override Timing.TimeSignature.denominator-style = #'note

  \tempo "(default)"
  \time 6/8
  a'2.

  \tempo "flat"
  \override Timing.TimeSignature.note-flag-style = #'flat-flag
  \time 6/8
  a'2.

  \tempo "mensural"
  \override Timing.TimeSignature.note-flag-style = #'mensural
  \time 6/8
  a'2.
}
