
\version "2.12.0"
\header {
    texidoc = "Once properties take effect during a single time step only."
}

\layout { ragged-right = ##t }

\relative c' {
    c4
    \once \override Stem #'thickness = #5.0
    c4
    c4
    c4
}
 

