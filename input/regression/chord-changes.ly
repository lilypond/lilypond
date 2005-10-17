\version "2.7.13"

\header{
  texidoc="Property chordChanges: display chord names only when
there's a change in the chords scheme, but always display the
chord name after a line break.
"
}

\layout{ raggedright = ##t }


scheme = \chordmode {
  c1:m \break c:m c:m c:m d
  c1:m \break c:m c:m c:m d
}

settings = {
}

<<
  \context ChordNames <<
    \scheme
    \set chordChanges = ##t
  >>
  \context Staff \transpose c c' \scheme
>>

