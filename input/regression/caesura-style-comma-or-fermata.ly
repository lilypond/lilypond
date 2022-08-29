\version "2.23.13"

\header {
  texidoc="This test customization of @code{\\caesura}.  In mid
measure, the caesura appears as a comma outside the staff.  At a bar
line it appears as a fermata."
}

testArticulation = \henzelongfermata

\include "caesura-style.ily"

\new Score \with {
  caesuraType = #'((breath . spacer) (scripts . (outsidecomma)))
  caesuraTypeTransform = #(at-bar-line-substitute-caesura-type
                           '((scripts . (fermata))))
} \music
