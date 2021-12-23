\version "2.23.6"

\header {
  texidoc = "Outside-staff positioning correctly takes
balloons into account."
}

\new Staff \with {
  \consists Balloon_engraver
} {
  \stemDown e2 s2
  \balloonGrobText SustainPedal #'(0 . -3) \markup
    "Very long balloon text"
  a'2\sustainOn a'\sustainOff
}
