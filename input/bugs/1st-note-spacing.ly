\header {
texidoc="apparently added in 1.5.2 -- what about it?"
}
\version "1.3.148"

\score {\notes \relative c'' <
\context Staff = SA { \times 6/7 { [c8 c c c c c c] }  }
\context Staff = SB { \times 6/6 { [c c c c c c] }  }
>

\paper { linewidth = -1. }
} 
