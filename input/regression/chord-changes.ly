\header{
texidoc="property chordChanges: only display chord names when
there's a change in the chords scheme, but always display the
chord name after a line break.
";
}

scheme = \chords {
  c1:m \break c:m c:m c:m d
  c1:m \break c:m c:m c:m d
}

settings = {
  \property ChordNames.chordChanges = ##t
}

\score {
  \notes <
    \context ChordNames < \scheme \settings >
    \context Staff \transpose c'' \scheme
  >
  \paper{
    linewidth = 40 * \staffspace;
  }
}
