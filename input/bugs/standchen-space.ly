\version "1.3.148"
\header{

    texidoc= "The 8th notes should have regular spacing. (TODO: check
    with printed ed.) "


}
\score {
\notes \relative c' <
\context Staff = SA  { c4. c8 \times 2/3 { [c8 c c] } }
\context Staff = SB  { [c8 c c c c c] }
>

\paper { linewidth = -1. }
}
