\version "2.23.13"

\header {
  texidoc="Context modifications can make @code{\\caesura} appear as a
stack of scripts.  In this case, the caesura itself is engraved as a
fermata over a comma, and a double-dot fermata is added as an
articulation.  The final caesura is colored red with @code{\\tweak
@dots{} \\caesura @dots{}}, which affects both the fermata and the
comma, but not the additional articulation."
}

\new Score \with {
  caesuraType = #'((breath . spacer)
                   (scripts . (outsidecomma fermata)))
} \fixed c' {
  e2
  \caesura
  f2
  \caesura
  g2
  \caesura \henzelongfermata
  a2
  \tweak color #red \caesura \henzelongfermata
}
