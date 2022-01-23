\version "2.23.6"

\header {
  texidoc = "This shows the one-voice rest positions for various
standard and tab staves."
}

makeStaffMusic =
#(define-music-function (music) (ly:music?)
  music)

\include "rest-positioning-score.ily"
