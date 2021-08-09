
\header {
  texidoc = "Downstem notes following a bar line are
printed with some extra space.  This is an optical correction similar
to juxtaposed stems.

The bar upstem should be approx 1.1 staff space, the bar downstem 1.3
to 1.5 staff space."
}


\version "2.19.21"
\layout{
  ragged-right = ##t 
}


\relative {
  \override Score.NonMusicalPaperColumn.stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn.layer = #2
  
  \time 2/4
  \stemDown
  s2
  e''4 s4 
  e,4 s4
}
