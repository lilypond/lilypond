\version "2.12.0"
\header {

  texidoc="In feathered beams, stems in knees reach up to the feathered part correctly.
"

}

\layout { ragged-right = ##t}

\relative c' {
  \override Beam #'grow-direction = #-1
%  \override Beam #'transparent = ##t
  \override Stem #'direction = #UP
  c32[
  \override Stem #'direction = #DOWN

  c''32 c32
  \override Stem #'direction = #UP
  c,,32]

  \override Beam #'grow-direction = #1
%  \override Beam #'transparent = ##t
  \override Stem #'direction = #DOWN
  c''32[
  \override Stem #'direction = #UP

  c,,32 c32
  \override Stem #'direction = #DOWN
  c''32]

}
