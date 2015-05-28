
\version "2.19.21"

\header {
  texidoc = "Accidentals do not influence the amount of stretchable space.
The accidental does add a little non-stretchable space. 
"
}

%% not ragged-right!!
\layout {
  line-width = 18.\cm
}
\relative {
  \accidentalStyle piano-cautionary
  \time 2/4 
  d''16 d d d d d cis d dis dis dis dis 
  
}
