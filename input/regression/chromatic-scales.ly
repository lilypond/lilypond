\version "2.21.0"
\header{
  texidoc="@code{staffLineLayoutFunction} is used to change the position of the notes.
This sets @code{staffLineLayoutFunction} to @code{ly:pitch-semitones} to
produce a chromatic scale with the distance between a consecutive
space and line equal to one semitone.
"
}

scales = \relative {
  a ais b c cis d dis e f fis g gis
  a
}

\new Staff \with {
  \remove "Accidental_engraver"
  \remove "Key_engraver" 
  staffLineLayoutFunction = #ly:pitch-semitones
  middleCPosition = #-6
  clefGlyph = "clefs.G"
  clefPosition = #(+ -6 7)
}
{
  \override Staff.StaffSymbol.line-count = #5
  \time 4/4
  <<
    \scales
    \context NoteNames {
      \set printOctaveNames= ##f
      \scales
    }
  >>
}
