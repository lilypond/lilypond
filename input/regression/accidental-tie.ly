\version "2.14.0"

\header {
  texidoc = "The second and third notes should not get accidentals,
    because they are tied to a note.  However, an accidental is
    present if the line is broken at the tie, which happens for the G
    sharp.

    The presence of an accidental after a broken tie can be
    overridden.
"
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
  \override Accidental #'hide-tied-accidental-after-break = ##t
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
