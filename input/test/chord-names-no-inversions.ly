\version "1.9.4"
\header { texidoc= "@cindex Chord Names No Inversions
Chord names don't attempt to find inversions and
bass notes. " }

scheme = \notes {
  <c' e' g'>1
  <e' g' c''>
  <e e' g' c''>
}

\score {
  <<
    \context ChordNames \scheme
    \context Staff \scheme
  >>
  \paper { raggedright = ##t}
}

