
\version "2.3.17"
\header{
texidoc = "Broken crescendi should be open on one side."
}

\score {  \relative c'' { 
    c1 \< \break c1\!  \> \break c1\!
  }
  \paper {
    linewidth = 4.\cm
  }
}
  

