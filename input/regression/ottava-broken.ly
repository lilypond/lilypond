
\header
{
    
texidoc = "Ottava brackets behave properly at line breaks: no vertical
line, and the horizontal line doesn't stick out."


}
\version "1.7.19"


\paper { raggedright = ##t} 
\score {
  \notes\relative c''' \notes {
  a2 b
  #(set-octavation 1)
  a2 b \break c a
  #(set-octavation 0)
  a b c a 
}
}

