\version "2.11.53"

\header {
  texidoc = "Cautionary accidentals applied to tied notes after a
bar line are valid for the whole measure."
}

notes = \relative c' {
  fis1 ~
  fis!2 fis ~
  fis?2 fis
}

<<
  \new NoteNames \notes
  \new Staff \notes
>>
