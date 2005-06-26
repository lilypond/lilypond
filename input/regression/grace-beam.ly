
\header
{
texidoc = "Grace beams and normal beams may occur simultaneously.
Unbeamed grace notes are not put into normal beams.
"
}

    \layout { raggedright= ##t }


\version "2.6.0"
\score { \relative c'' { c4  d8[ \grace {  e32[  d c d] } e8]  e[  e \grace { f16 } e8 e]   }}

