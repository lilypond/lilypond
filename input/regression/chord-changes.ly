\version "2.3.17"

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
  \set chordChanges = ##t
}

\score {
   <<
    \context ChordNames << \scheme \settings >>
    \context Staff \transpose c c' \scheme
  >>
  \paper{
    linewidth = 40 * \staffspace
  }
}
