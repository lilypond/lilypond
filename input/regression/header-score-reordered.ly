\version "2.19.0"
\header {
  texidoc="
Header blocks may appear before and after the actual music in a score.
"
}

\markup \vspace #3
\markup { \bold Note: expect piece and opus. }
\markup \vspace #3

\score {
  \header {
    piece = "Piece correct (set in score)"
    opus = "Opus incorrect (to be superseded at score level)"
  }
  \new Staff { c'1 }
  \header {
    % This should NOT overwrite the piece from above!
    opus = "Opus correct (superseded at score level)"
  }
}
