\version "2.2.0"
\header{
texidoc="crashes lily"
}

\score { \notes {
  \context Voice {
    \set crescendoText = "cresc."
    \set crescendoSpanner = #'dashed-line
    c2. r8 c8 \< \break \! c1
  }
}
\paper { linewidth = 5.\cm}
}

