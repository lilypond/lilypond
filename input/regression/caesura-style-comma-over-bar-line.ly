\version "2.23.13"

\header {
  texidoc="Context modifications can make @code{\\caesura} appear as a
comma outside the staff.  In this case, when the caesura comes at a
measure boundary, the comma is aligned over the bar line rather than
like a breath mark."
}

%% Important: position of comma relative to bar line and fermata
#(set-global-staff-size 30)

testArticulation = \fermata

\include "caesura-style.ily"

\new Score \with {
  caesuraType = #'((breath . spacer) (scripts . (outsidecomma)))
  caesuraTypeTransform = #(at-bar-line-substitute-caesura-type
                           '((scripts . (outsidecomma))))
} \music
