
\version "2.11.51"

\header {
  texidoc = "Accidentals do not influence the amount of stretchable space.
The accidental does add a little non-stretchable space. 
"
}

%% not ragged-right!!
\layout {
  line-width = 18.\cm
}
\relative c'' {
  #(set-accidental-style 'piano-cautionary)
  \time 2/4 
  d16 d d d d d cis d dis dis dis dis 
  
}
