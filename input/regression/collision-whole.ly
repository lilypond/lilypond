\version "2.19.21"

\header {
 texidoc = "Mixed collisions with whole and longer notes
 require asymmetric shifts."
}

\layout{ragged-right=##t}
\relative {
  \override Score.NoteHead.style = #'altdefault
  <<
    { c''1 c2*2 c1 c4*4 c1 c\breve*1/2 c\breve*1/2 b\breve*1/2 }
    \\
    { c2*2 c1 c4*4 c1 c\breve*1/2 c1 b\breve*1/2 c\breve*1/2 }
 >>
}
