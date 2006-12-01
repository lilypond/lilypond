\version "2.10.1"

\header {
 texidoc = "Mixed collisions with whole notes require asymmetric shifts."
}

\layout{ragged-right=##t}
\relative c'' {
  <<
    { c1 c2 s2 c1 c4 s2. }
    \\
    { c2 s2 c1 c4 s2. c1 }
 >>
}
