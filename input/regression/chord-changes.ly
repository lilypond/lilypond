\version "2.3.22"

\header{
texidoc="Property chordChanges: display chord names only when
there's a change in the chords scheme, but always display the
chord name after a line break.
"
}

scheme = \chordmode {
  c1:m \break c:m c:m c:m d
  c1:m \break c:m c:m c:m d
}

settings = {
}

\score {
    <<
	\context ChordNames <<
	    \scheme
	    \set chordChanges = ##t
	>>
	\context Staff \transpose c c' \scheme
    >>
    \layout{ raggedright = ##t }
}
