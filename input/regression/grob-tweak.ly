\header
{

  texidoc = "With the @code{\\tweak} function, individual grobs that
  are directly caused by events may be tuned directly."

}

\version "2.17.6"
\paper {
  ragged-right = ##t
}

{
  \set fingeringOrientations = #'(right)
  <
    \tweak font-size #3  c
    \tweak color #red  d-\tweak font-size #8 -4
    \tweak style #'cross  g
    \tweak duration-log #1  a
  >4
}
