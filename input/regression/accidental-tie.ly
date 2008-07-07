\version "2.11.51"

\header {
  texidoc = "The second and third notes should not get accidentals,
    because they are tied to a note.  However, an accidental is
    present if the line is broken at the tie, which happens for the G
    sharp."

}
\layout {
  ragged-right = ##t
}

mus =  	\relative c' {
  f1~
  f2~f4 % ~ f8
  fis8  gis8 ~
  \break
  gis1
}

<<
  \new NoteNames \mus
  \new Voice { \key g \major \mus }
>>
