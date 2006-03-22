\version "2.8.0"
\header{
texidoc="crashes lily"
}

\score {  {
  \context Voice {
    \set crescendoText = "cresc."
    \set crescendoSpanner = #'dashed-line
    c2. r8 c8 \< \break \! c1
  }
}
\layout { line-width = 5.\cm}
}

