\version "1.9.0"


\header {
texidoc= "Grace notes and multi-measure rests."
}

\score   {
\notes <
	\context Staff = SB { R1 R1 R1*3 }
	\context Staff = SA { \clef bass c1 \grace c8 c2 c2 c1  \grace c16 c2 c2 c1 }
>
}
