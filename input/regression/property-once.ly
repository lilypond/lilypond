
\version "2.3.22"
\header {
    texidoc = "Once properties take effect during a single time step only."
}

\layout { raggedright = ##t }

\relative c' {
    c4
    \once \set Stem = #'()
    c4
    \once \override Stem  #'thickness = #5.0
    c4
    c4
}
 

