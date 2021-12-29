\version "2.23.6"

\header {
  texidoc = "@code{BalloonText} has configurable thickness."
}

\new Voice \with {
  \consists Balloon_engraver
}
{
  \override BalloonText.thickness = 2
  \override BalloonText.padding = 1
  \balloonGrobText NoteHead #'(1 . -1) "note head"
  c'1
}
