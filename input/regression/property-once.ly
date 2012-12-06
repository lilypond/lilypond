
\version "2.17.6"
\header {
    texidoc = "Once properties take effect during a single time step only."
}

\layout { ragged-right = ##t }

\relative c' {
    c4
    \once \override Stem.thickness = #5.0
    c4
    c4
    c4
}
 

