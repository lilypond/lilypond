\version "2.23.6"

\header {
  texidoc = "Stroke fingerings don't segfault when their stencil is
set to @code{##f}."
}

{
  \set strokeFingerOrientations = #'(right)
  \once \omit StrokeFinger
  g'\rightHandFinger 2
  \once \omit StrokeFinger
  g'\rightHandFinger 2 \rightHandFinger 3
  g'\single \omit StrokeFinger \rightHandFinger 2 \rightHandFinger 4
  \once \omit StrokeFinger
  <g'\rightHandFinger 2>
  \once \omit StrokeFinger
  <g'\rightHandFinger 2 c''\rightHandFinger 4>
  <g'\single \omit StrokeFinger \rightHandFinger 2 c''\rightHandFinger 4 >
}
