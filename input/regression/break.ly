
\version "2.23.14"

\header{
  texidoc="
Breaks can be encouraged and discouraged using @code{\\break} and
@code{\\noBreak}."
}

\layout {
  indent = 0.0
  line-width = 4.0\cm
}

\relative c'' \context Voice {
  \textLengthOff
  c1 \noBreak c1 \noBreak \textMark "nobreak" c1 \noBreak
  c1 \break \textMark "break" c1 \break \textMark "break" c1
}
