

\header
{
    
texidoc = "Ottava brackets behave properly at line breaks: no vertical
line, and the horizontal line doesn't stick out."


}
\version "2.1.22"


\paper { raggedright = ##t} 
\score {
  \notes\relative c''' \notes {
  a2 b
  #(set-octavation 1)
  a2 b \break c''1 \break
  a
  #(set-octavation 0)
  g,,2 b c a 
}
}

