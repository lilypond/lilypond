\header {
  texidoc = "Colliding note-columns may be shifted manually
with @code{force-hshift}.  Arrangements of notes after
collision-resolution have their main columns (not suspended notes)
left-aligned, excluding columns with forced shifts."
}
\version "2.19.24"

\new PianoStaff \with { \consists #Span_stem_engraver } <<
  \new Staff \fixed c' <<
    \new Voice {
      \voiceOne f e
      \crossStaff
      \once\override NoteColumn.force-hshift = #-0.8
      f
      \once\override NoteColumn.force-hshift = #-1.5
      e
    }
    \new Voice {
      \voiceTwo e f
      \once\override NoteColumn.force-hshift = #0
      \crossStaff e
      \crossStaff f
    }
  >>
  \new Staff {\clef bass g g e f }
>>

