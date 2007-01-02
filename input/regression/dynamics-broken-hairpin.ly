
\version "2.10.0"
\header{
texidoc = "Broken crescendi should be open on one side."
}

\layout {
  line-width = 4.\cm
}


\relative c'' { 
  c1 \< \break c1\!  \> \break c1\!
}

