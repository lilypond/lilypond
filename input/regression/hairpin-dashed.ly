\version "2.19.21"

\header {
  texidoc ="Hairpin crescendi may be dashed. "

}


\relative {
  \override Hairpin.style = #'dashed-line
  f'2\< g c1 d4\> b a gis\! }     

