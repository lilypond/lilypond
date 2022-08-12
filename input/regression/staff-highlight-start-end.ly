\version "2.23.12"

\header {
  texidoc = "This test shows highlights starting and ending on
prefatory material in various situations."
}

{
  % Start a little bit after the C clef
  \staffHighlight lightsteelblue
  c'8 g' e'' g' e''2
  % End on the bar line
  \staffHighlight pink
  c'8 g' e'' g' e''2
  \staffHighlight lightsteelblue
  \clef bass
  c8 g e' g
  \clef treble
  % End before the clef and start after it, with some padding on both sides
  \staffHighlight pink
  e'2
  % The padding can be increased, but it is limited by the notes.
  \override Staff.StaffHighlight.bound-prefatory-paddings = #'(2 . 2)
  \staffHighlight lightsteelblue
  \clef bass
  c8 g e' g
  \clef treble
  \staffHighlight pink
  e'2
  % Padding is not limited by skips, however.
  \override Staff.StaffHighlight.bound-prefatory-paddings = #'(2 . 2)
  \staffHighlight lightsteelblue
  \clef bass
  c8 g s4
  \clef treble
  \staffHighlight pink
  s2
}
