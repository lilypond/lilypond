\header {
  title="Chord Taxonomy of LilyPond -- jazz"
}
scheme = \chords {
      c1
      c:4
      c:9
      bes:9^7
      c:11^7
      c:9+
      % TODO
      c:m5-.7
      c:m5-.7-
      c:7+
      c:m7+
    }
    
\score {
  <
    \property ChordNames.ChordName \set #'style = #'jazz
    \context ChordNames \scheme
    \context Staff \notes \transpose c'' \scheme
  >
}
