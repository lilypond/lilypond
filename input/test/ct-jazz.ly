\version "1.5.68"
\header {
  title="Chord Taxomony of LilyPond -- jazz"
  subtitle="Amy's chord tests"
}
scheme = \chords {
      c1
      c:4
      c:9
      bes:9^7
      c:11^7
      c:9+
      % TODO
    }
    
\score {
  <
    \property ChordNames.ChordName \set #'style = #'jazz
    \context ChordNames \scheme
    \context Staff \notes \transpose c'' \scheme
  >
}
