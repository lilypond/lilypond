\header {
  texidoc = "Do not crash on handling round-filled-box with infinite extents."
}

\version "2.21.0"

{
  b1-\markup {
    \box \vspace #1
  }
}
