\header {
texidoc = "Pieces may begin with grace notes."
}
\score  {\notes \relative c' \context Staff  { 
 \grace { [a'16 f]  } g1
 \grace { [a16 bes]  }  c1
  }
  \paper { linewidth = -1. }
}
