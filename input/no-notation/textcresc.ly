\version "1.3.148"
\header{
texidoc="crashes lily"
}

\score { \notes {
  \context Voice {
    \property Voice.crescendoText = "cresc."
    \property Voice.crescendoSpanner = #'dashed-line
    c2. r8 c8 \< \break \! c1
  }
}
\paper { linewidth = 5.\cm}
}

