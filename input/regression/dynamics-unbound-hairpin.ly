\version "2.7.13"

\header {
texidoc = "Crescendi may start off-notes, however, they should  not collapse into flat lines."
}
\layout { raggedright = ##t}


\context  Voice { 
  << f''1 { s4 s4 \< s4\! \> s4\! } >>
}



