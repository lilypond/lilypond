\version "2.23.6"

\header {
  texidoc = "The alignment of a balloon text can be customized as
well as the attachment point of the line connecting it to the
frame."
}

\new Voice \with {
  \consists Balloon_engraver
}
{
  \once \override BalloonText.X-attachment = #CENTER
  \once \override BalloonText.text-alignment-Y = -0.8
  \balloonGrobText NoteHead #'(5 . -2) "note head"
  c'2.
  \once \override BalloonText.Y-attachment = #CENTER
  \once \override BalloonText.text-alignment-X = #CENTER
  \balloonGrobText Rest #'(1 . 3.5) "rest"
  r4
}
