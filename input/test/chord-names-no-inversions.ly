\version "2.3.4"
\header { texidoc= "@cindex Chord Names No Inversions
Since there are several interpretations for recognizing 
chord names, the lowest note is the bass note of a chord and the inversion 
of the chord is found accordingly. " }

scheme =  {
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

