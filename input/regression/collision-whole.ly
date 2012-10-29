\version "2.17.6"

\header {
 texidoc = "Mixed collisions with whole and longer notes
 require asymmetric shifts."
}

\layout{ragged-right=##t}
\relative c'' {
  \override Score.NoteHead.style = #'altdefault
  <<
    { c1 c2*2 c1 c4*4 c1 c\breve*1/2 c\breve*1/2 b\breve*1/2 }
    \\
    { c2*2 c1 c4*4 c1 c\breve*1/2 c1 b\breve*1/2 c\breve*1/2 }
 >>
}
