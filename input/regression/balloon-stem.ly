\version "2.23.6"

\header {
  texidoc = "Balloons work on beamed stems."
}

\new Voice \with {
  \consists Balloon_engraver
}
{
  \balloonLengthOn
  \override BalloonText.padding = 0.3
  \balloonGrobText Stem #'(-1 . 2) "stem"
  d'8[ 8]
}
