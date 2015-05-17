
\version "2.19.21"

\header{
texidoc="
Stems, beams, ties and slurs should behave similarly, when placed
on the middle staff line. Of course stem-direction is down for high
notes, and up for low notes.
"
}

\layout {
    ragged-right = ##t
}  
\context Voice \relative {
    b'4 ~ 8(b8) e4 e,
    
}

