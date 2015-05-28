
\header {
  texidoc = "An accidental following a bar gets space so
 the left edge of the acc is at 0.3 staff space from the bar line"
}


\version "2.19.21"
\layout{
  ragged-right = ##t 
}


\relative {
  \override Score.NonMusicalPaperColumn.stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn.layer = #2
  
  \time 2/4
  \stemUp
  s2
  c''4 r4
  cis4 r4 
}
