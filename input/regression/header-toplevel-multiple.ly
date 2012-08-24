\version "2.16.0"
\header {
  texidoc="
A second top-level header block shall not entirely replace a first header block, but only changed variables.
"
  piece = "Piece correct (set at top level)"
  title = "Title incorrect (to be superseded at top level)"
}
\header {
  % This should NOT overwrite the piece from above!
  title = "Title correct (superseded at top level)"
}
\markup \vspace #3
\markup { \bold Note: expect title and piece. }
\markup \vspace #3

\score {
  \new Staff { c'4 }
}
