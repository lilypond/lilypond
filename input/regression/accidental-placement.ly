\version "1.5.68"
\header {
    texidoc ="Accidentals are placed as closely as possible.
Accidentals in corresponding octaves are aligned.
The top accidental should be nearest to the chord. The
flats in a sixth shoudl be staggered.  "
}


\score { \notes \context Voice \relative c' {
    cis4
    c4
 \transpose c'' {    
  <bis4 es gis>
  <es! as!>
  <gis! cis!>
  <g! des'>
  <ges! es'!>
}    
    <cis4 d es fis gis ases bes ces d dis >
    <bes'! fis!>     
    <bes! cis!>
    <c! es ges beses>
    <bes! c d f >    
    <bes,! c d f >
}
\paper { linewidth = -1. }
     }
