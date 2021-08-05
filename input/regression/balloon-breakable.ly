\version "2.23.4"

\header {
  texidoc = "Balloons on breakable items are visible if and only
if the item they annotate is visible."
}

\new Staff \with {
  \consists Balloon_engraver
}
{
  \key cis \major
  c1
  \balloonGrobText
    KeyCancellation
    #'(0 . 2)
    \markup \center-column { key cancellation }
  \balloonGrobText
    KeySignature
    #'(0 . -2)
    \markup \center-column { key signature }
  \key ces \major
  \break
  c1
}
