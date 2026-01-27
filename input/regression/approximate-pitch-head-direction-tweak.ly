\version "2.25.33"

\header {
  texidoc  = "@code{ApproximatePitchNoteHead@/.direction} can be tweaked to use
the glyph that is normally used on a stem of the given direction, irrespective
of the associated stem's @code{direction}.  (The result is counterintuitive for
heads in the @code{arrow} style, but that is the convention.)

In each measure, the first head uses the default direction, the second uses
@code{DOWN} and the third uses @code{UP}."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 30) % magnify differences in note heads

\new Staff \with {
  \remove Clef_engraver
  \remove Time_signature_engraver
} {
  \time 3/4

  <>^\markup \teeny "Stems neutral"
  \approximatePitch c'4
  \tweak ApproximatePitchNoteHead.direction #DOWN \approximatePitch 4
  \tweak ApproximatePitchNoteHead.direction #UP \approximatePitch 4

  \approximatePitch b'4
  \tweak ApproximatePitchNoteHead.direction #DOWN \approximatePitch 4
  \tweak ApproximatePitchNoteHead.direction #UP \approximatePitch 4

  \approximatePitch a''4
  \tweak ApproximatePitchNoteHead.direction #DOWN \approximatePitch 4
  \tweak ApproximatePitchNoteHead.direction #UP \approximatePitch 4

  <>_\markup \teeny "Stems up"
  \stemUp
  \approximatePitch b'4
  \tweak ApproximatePitchNoteHead.direction #DOWN \approximatePitch 4
  \tweak ApproximatePitchNoteHead.direction #UP \approximatePitch 4
  \stemNeutral
}
