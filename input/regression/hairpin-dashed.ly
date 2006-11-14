\version "2.10.0"

\header {
  texidoc ="Hairpin crescendi may be dashed. "

}


\relative c' {
  \override Hairpin  #'dash-fraction = #0.4
  \override Hairpin  #'dash-period = #1
  f2\< g c1 d4\> b a gis\! }     

