
\version "2.3.22"

\header{
texidoc="
Beams, stems and noteheads often have communication troubles, since
the two systems for y dimensions (1 unit = staffspace, 1 unit = 1
point) are mixed.

Stems, beams, ties and slurs should behave similarly, when placed
on the middle staff line. Of course stem-direction is down for high
notes, and up for low notes.
"
}

\layout {
    raggedright = ##t
}  
\context Voice \relative c {
    b''4 ~ b8(b8) e4 e,
    
}

