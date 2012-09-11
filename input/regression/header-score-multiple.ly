\version "2.16.0"
\header {
  texidoc="
A second score-level header block shall not entirely replace a first header block, but only update changed variables.
"
}

\markup \vspace #3
\markup { \bold Note: expect piece and opus. }
\markup \vspace #3

\score {
  \new Staff { c'1 }
  \header {
    piece = "Piece correct (set in score)"
    opus = "Opus incorrect (to be superseded at score level)"
  }
  \header {
    % This should NOT overwrite the piece from above!
    opus = "Opus correct (superseded at score level)"
  }
}
