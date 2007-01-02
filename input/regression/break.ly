
\version "2.10.0"

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
  \emptyText
  c1 \noBreak c1 \noBreak \mark "nobreak" c1 \noBreak
  c1 \break \mark "break" c1 \break \mark "break" c1 
}
