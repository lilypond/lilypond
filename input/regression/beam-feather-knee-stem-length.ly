\version "2.19.21"
\header {

  texidoc="In feathered beams, stems in knees reach up to the feathered part correctly.
"

}

\layout { ragged-right = ##t}

\relative {
  \override Beam.grow-direction = #-1
%  \hide Beam
  \override Stem.direction = #UP
  c'32[
  \override Stem.direction = #DOWN

  c''32 c32
  \override Stem.direction = #UP
  c,,32]

  \override Beam.grow-direction = #1
%  \hide Beam
  \override Stem.direction = #DOWN
  c''32[
  \override Stem.direction = #UP

  c,,32 c32
  \override Stem.direction = #DOWN
  c''32]

}
