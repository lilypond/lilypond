\header{
texidoc="property chordChanges: only display chord names when
there's a change in the chords scheme, but always display the
chord name after a line break.
";
}

scheme = \chords {
%  \property ChordNames.chordChanges = ##t
  c1:m \break c:m c:m c:m d
}

\score {
  \notes <
    \context ChordNamesVoice \scheme
    \context Staff \transpose c'' \scheme
  >
  \paper{
    linewidth = 40 * \staffspace;
  }
}
