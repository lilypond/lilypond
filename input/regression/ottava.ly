
\header
{
texidoc = "Ottava brackets are supported, through the
use of the scheme function @code{set-octavation}. 
"

}
\version "2.1.7"


\paper { raggedright = ##t} 
\score {
  \notes\relative c''' \notes {
  a b c a
  #(set-octavation 2)
  a' b
  c a
  #(set-octavation 0)

  a, #(set-octavation 1) b
  #(set-octavation 0)
  c a 
  #(set-octavation -1) b a g 
  #(set-octavation 0)
  c 
}
}

