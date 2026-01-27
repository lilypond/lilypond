\version "2.25.33"

\header {
  texidoc = "@code{NoteHead@/.direction} can be overridden to use the glyph that
is normally used on a stem of the given direction, irrespective of the
associated stem's @code{direction}.

The stem is not required to adjust itself to remain attached to the head.

In each measure, the first note uses the default, the second note uses
@code{DOWN} and the third uses @code{UP}."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 30) % magnify differences in note heads

\new Staff \with {
  \remove Clef_engraver
  \remove Time_signature_engraver
  \aikenHeads
} {
  \time 3/2

  f'2
  \tweak NoteHead.direction #DOWN 2
  \tweak NoteHead.direction #UP 2

  f''2
  \tweak NoteHead.direction #DOWN 2
  \tweak NoteHead.direction #UP 2
}
