\version "2.23.6"

\header {
  texidoc = "String numbers don't segfault when their stencil is
set to @code{##f}."
}

{
  \set stringNumberOrientations = #'(right)
  \once \omit StringNumber
  g'-\1
  \once \omit StringNumber
  g'-\1-\3
  g'\single \omit StringNumber -\1 -\3
  \once \omit StringNumber
  <g'-\2>
  \once \omit StringNumber
  <g'-\2 c''-\4 >
  <g'\single \omit StringNumber -\2 c''-\4>
}
