

\header
{
    
texidoc = "At line breaks, ottava brackets have no vertical line and 
their horizontal line does not stick out."


}
\version "2.3.4"


\paper { raggedright = ##t} 
\score {
  \relative c'''  {
  a2 b
  #(set-octavation 1)
  a2 b \break c''1 \break
  a
  #(set-octavation 0)
  g,,2 b c a 
}
}

