
\version "2.11.51"
\header{
  texidoc="
A sharp sign after a double sharp sign, as well as a flat sign
after a double flat sign is automatically prepended with a
natural sign.
"
}


\layout { ragged-right = ##t }

thenotes =  \relative cis' {
  \time 4/4
  gisis'4 gis geses ges |
}

<<
  \context Staff \thenotes
  \context NoteNames  {
    \override NoteNames.NoteName   #'no-spacing-rods = ##f 
    \thenotes
  }
>>


