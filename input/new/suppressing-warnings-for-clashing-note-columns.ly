\version "2.11.49"
\header {
  lsrtags = "simultaneous-notes,tweaks-and-overrides"
  texidoc = "
If notes from two voices with stems in the same direction are
placed at the same position, and both voices have no shift or the
same shift specified, the error message \"warning: ignoring too
many clashing note columns\" will appear when compiling the
LilyPond file.  This message can be suppressed by setting the
@code{'ignore-collision} property of the @code{NoteColumn} object
to @code{#t}.
"
  doctitle = "Suppressing warnings for clashing note columns"
}

ignore = \once \override NoteColumn #'ignore-collision = ##t

\relative c' <<
  { \oneVoice \stemDown f2\glissando \stemNeutral a } \\
  { \oneVoice \ignore c2\glissando \ignore d, }
>>
