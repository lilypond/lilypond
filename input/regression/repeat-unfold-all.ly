
\header {
    texidoc = "Repeats may be unfolded through the Scheme function @code{unfold-repeats}."
}

nots = \notes\relative c'   {c4 \repeat volta 2 c4 \alternative { d e  }}

\score { \notes {
\nots
\apply #unfold-repeats \nots
}
\paper {linewidth=-1.} 
     }
