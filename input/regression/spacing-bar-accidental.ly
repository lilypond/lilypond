
\header {
  texidoc = "An accidental following a bar gets space so
 the left edge of the acc is at 0.3 - 0.6 staff space of the bar line"
}


\version "2.11.51"
\layout{
  ragged-right = ##t 
}


\relative c'' {
  \override Score.NonMusicalPaperColumn #'stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn #'layer = #2
  
  \time 2/4
  \stemUp
  s2
  c4 r4
  cis4 r4 
}
