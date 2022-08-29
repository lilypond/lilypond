\version "2.23.13"

\header {
  texidoc="This test customization of @code{\\caesura}.  In mid
measure, the caesura appears as a comma outside the staff.  At a bar
line, no caesura mark appears, but optional articulations still
appear."
}

testArticulation = \fermata

\include "caesura-style.ily"

\new Score \with {
  caesuraType = #'((breath . spacer) (scripts . (outsidecomma)))
  caesuraTypeTransform = #(at-bar-line-substitute-caesura-type '())
} \music
