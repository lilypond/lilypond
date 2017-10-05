
\header {

  texidoc = "Frets can be assigned automatically. The results will be
best when one string number is indicated in advance "
  
}

\version "2.21.0"

foo = \relative {
  <d\5 fis a d fis>_\markup {
    \fret-diagram-terse "x;5;4;2;3;2;"
  }
}

<<
  \new FretBoards {
    \set instrumentName = "autofrets"
    \foo

  }
  \new Staff {
    \clef "G_8"
    \set stringNumberOrientations = #'(left)
    \set fingeringOrientations = #'(right)
    \foo
  }

>>


