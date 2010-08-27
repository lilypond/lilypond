\header {

  
  texidoc = "@code{\\tempo} are aligned with Tempo, Key or first musical
element.

"
  
}
\version "2.13.24"

\layout {
  line-width = 50\mm
}
\relative {
  \tempo "T-first" c1\mark \default \tempo "T-note" c \break
  \tempo "T-break" c \tempo "T-rest" R
  \break
  \time 8/8 \tempo "T-time" R
  \key as \major \tempo "T-key" R
}
