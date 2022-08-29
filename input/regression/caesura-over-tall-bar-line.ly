\version "2.23.13"

\header {
  texidoc="A caesura script is automatically shifted up to avoid
colliding with a tall bar line."
}

\new Staff \with {
  caesuraType = #'((scripts . (fermata)))
} {
  R1 \caesura \inStaffSegno R1
}
