\header{
texidoc="property chordChanges: only display chord names when
there's a change in the chords scheme, but always display the
chord name after a line break.
";
}

scheme = \chords {
 % ugh ugh: this breaks the output of notes in the regular staff 
 \property ChordNamesVoice.chordChanges = ##t
  % and this even breaks the chord names 
   \property ChordNames.chordChanges = ##t
  c1:m \break c:m c:m c:m d
  c1:m \break c:m c:m c:m d
}

\score {
  \notes <
    \context ChordNames \scheme
    \context Staff \transpose c'' \scheme
  >
  \paper{
    linewidth = 40 * \staffspace;
    \translator {
      \ChordNamesVoiceContext
      chordChanges = ##t
    }
  }
}
