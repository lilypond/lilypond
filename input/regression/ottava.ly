#(ly:set-option 'old-relative)
\header
{
texidoc = "Ottava brackets are supported, through the
use of the scheme function @code{set-octavation}. 
"

}
\version "1.9.0"


\paper { raggedright = ##t} 
\score {
  \notes\relative c''' \notes {
  a b c a
  #(set-octavation 1)
  a b c a
  #(set-octavation 0)

  a #(set-octavation 1) b
  #(set-octavation 0)
  c a 
}
}

