\version "2.25.0"

\header {
  texidoc = "@code{\\tweak} takes effect on repeat chords @samp{q}
just as if the repetition were written out.  In this test, the two
first scores should be identical, and the third score should be identical
to the first two except for two added dynamic markings.  The stems should
be thicker than usual, and note heads in the second chord and the third
one should be bigger."
}

{
  \tweak Stem.thickness 5 <c' c''>\p
  \tweak font-size 3 \tweak Stem.thickness 5 <c' c''>\p~
  \tweak font-size 3 \tweak Stem.thickness 5 <c' c''>\p
}

{
  \tweak Stem.thickness 5 <c' c''>\p
  % New tweak is added, Stem.thickness tweak is carried over.
  % Tie does not have its font-size tweaked, just as in
  % the previous score.
  \tweak font-size 3 q~
  \tweak font-size 3 q
}

\chordRepeats dynamic-event {
  \tweak Stem.thickness 5 <c' c''>\p
  % Same here, DynamicText does not get a font-size tweak.
  \tweak font-size 3 q~
  \tweak font-size 3 q
}
