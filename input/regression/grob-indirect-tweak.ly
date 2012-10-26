\header
{

  texidoc = "With the full form of the @code{\\tweak} function,
individual grobs that are indirectly caused by events may be tuned."

}

\version "2.17.6"
\paper {
  ragged-right = ##t
}

\relative c''
{
  <
    \tweak Accidental.color #red   cis4
    \tweak Accidental.color #green es
    g
  >4
}
