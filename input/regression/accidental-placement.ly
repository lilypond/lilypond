
\version "2.19.21"

\header {

  texidoc ="Accidentals are placed as closely as possible.
Accidentals in corresponding octaves are aligned.
The top accidental should be nearest to the chord. The
flats in a sixth should be staggered.  "
  
}

\layout {
  ragged-right = ##t
}


\context Voice \relative
{
  <d' e! bes'!>4
  cis4
  c4
  \transpose c c' {
    <ges es'>
    <bis es gis>4
    <es! as!>
    <gis! cis!>
    <g! des'>
    <ges! es'!>
  }    
  <cis d es fis gis ases bes ces d e! >4
  <bes'! fis!>     
  <bes! cis!>
  <c! es ges beses>
  <bes! c d f >    
  <bes,! c d f >

}


