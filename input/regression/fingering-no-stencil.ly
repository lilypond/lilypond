\version "2.21.2"

\header {
  texidoc = "Fingerings don't segfault when their stencil is
set to @code{##f}."
}

{
  \set fingeringOrientations = #'(right)
  \once \omit Fingering
  g'-1
  \once \omit Fingering
  g'-1-3
  g'\single \omit Fingering -1 -3
  \once \omit Fingering
  <g'-2>
  \once \omit Fingering
  <g'-2 c''-4 >
  <g'\single \omit Fingering -2 c''-4>
}
