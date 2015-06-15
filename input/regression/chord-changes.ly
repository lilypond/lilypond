\version "2.19.22"

\header{
  texidoc="Property chordChanges: display chord names only when
there's a change in the chords scheme, but always display the
chord name after a line break.
"
}

\layout{ ragged-right = ##t }


scheme = \chordmode {
  c1:m \break c:m c:m c:m d
  c1:m \break c:m c:m c:m d
}

settings = {
}

<<
  \new ChordNames <<
    \scheme
    \set chordChanges = ##t
  >>
  \new Staff \transpose c c' \scheme
>>

