\version "2.16.0"

\header {
  lsrtags = "expressive-marks, staff-notation, tweaks-and-overrides"

  texidoc = "
Skipped beats in very long glissandi are sometimes indicated by
timing marks, often consisting of stems without noteheads.  Such
stems can also be used to carry intermediate expression markings.

If the stems do not align well with the glissando, they may need to
be repositioned slightly.
"

  doctitle = "Adding timing marks to long glissandi"
}

glissandoSkipOn = {
  \override NoteColumn #'glissando-skip = ##t
  \override NoteHead #'transparent = ##t
  \override NoteHead #'no-ledgers = ##t
}

glissandoSkipOff = {
  \revert NoteColumn #'glissando-skip
  \revert NoteHead #'transparent
  \revert NoteHead #'no-ledgers
}

\relative c'' {
  r8 f8\glissando
  \glissandoSkipOn
  f4 g a a8\noBeam
  \glissandoSkipOff
  a8

  r8 f8\glissando
  \glissandoSkipOn
  g4 a8
  \glissandoSkipOff
  a8 |

  r4 f\glissando \<
  \glissandoSkipOn
  a4\f \>
  \glissandoSkipOff
  b8\! r |
}
