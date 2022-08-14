\version "2.23.14"

\header {
  texidoc = "@code{\\markLengthOn} also works on text marks."
}

{
  \textMark "long mark text"
  c'4
  \markLengthOn
  \textMark "another long mark text"
  4
  \textMark "yet another very long mark text"
  4
}
