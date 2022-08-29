\version "2.23.13"

\header {
  texidoc="This test customization of @code{\\caesura}.  In mid
measure, the caesura appears as `railroad tracks'.  At a bar line it
appears as a fermata."
}

testArticulation = \henzelongfermata

\include "caesura-style.ily"

\new Score \with {
  caesuraTypeTransform = #(at-bar-line-substitute-caesura-type
                           '((scripts . (fermata))))
} \music
