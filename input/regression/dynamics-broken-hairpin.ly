\version "1.7.18"
\header{
texidoc = "Broken crescendi should be open on one side."
}

\score { \notes \relative c'' { 
    c1 \< \break \! c1  \> \break \! c1 
  }
  \paper {
    linewidth = 4.\cm
  }
}
  

