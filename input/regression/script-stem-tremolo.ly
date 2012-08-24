\header {

  texidoc = "Scripts avoid stem tremolos even if there is no visible stem."

}
\version "2.16.0"

\layout {ragged-right =##t}
{
  \stemDown
  g'1:32_"foo"
  g'1:32_.
  g'1:32_\f
}
