\version "2.12.0"

\header {
  texidoc = "The presence of an accidental after a broken tie can be
overridden."
}
\layout {
  ragged-right = ##t
}

mus =  	\relative c' {
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
