
\header
{
texidoc = "Ottava brackets are supported, through the
use of the scheme function @code{set-octavation}.

The spanner should go below for 8va bassa, and the string can be tuned
with @code{Staff.ottavation}.

"

}
\version "2.1.7"


\paper { raggedright = ##t} 
\score {
  \notes\relative c''' \notes {
  a b c a
  #(set-octavation 1)
  a b c a
  #(set-octavation 0)
  #(set-octavation 2)
  a b c a
  #(set-octavation 0)
  #(set-octavation -1)
  a b c a
  #(set-octavation 0)
  a,
  #(set-octavation 1)
  \property Staff.ottavation = #"8"
  b
  #(set-octavation 0)
  c a 
  #(set-octavation -1) b a g 
  #(set-octavation 0)
  c
}
}

