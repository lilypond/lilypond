\version "2.21.2"

\header {

  texidoc = "
    NoteNames and ChordNames contexts have (limited)
support for makam notation.  The alteration glyphs
displayed in these two contexts should be the same
as the ones on the staff.
  "
}

\include "makam.ly"

music = \relative {
  g afbm bfc cfk d efi fk g
}

<<
  \new Staff \music
  \new NoteNames \music
  \new ChordNames \music
>>
