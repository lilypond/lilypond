\version "1.3.148"
\header{
texidoc="what about this.    -> regression?"
}
\score {
\notes \relative c' <
\context Staff = SA  { c4. c8 \times 2/3 { [c8 c c] } }
\context Staff = SB  { c8 c c c c c }
>

\paper { linewidth = -1. }
}
