\header
{
texidoc = "ottava brackets are supported, through the
use of the scheme function @code{set-octavation}. 
"

}
\version "1.7.18"


\paper { raggedright = ##t} 
\score {
  \notes\relative c''' \notes {
  a b c a
  #(set-octavation 1)
  a b c a
  #(set-octavation 0)

  a b c a 
}
}

