
\version "2.19.21"
\header {
    texidoc = "Once properties take effect during a single time step only."
}

\layout { ragged-right = ##t }

\relative {
    c'4
    \once \override Stem.thickness = #5.0
    c4
    c4
    c4
}
 

