\version "2.16.0"

\header {
texidoc = "Crescendi may start off-notes, however, they should  not collapse into flat lines."
}
\layout { ragged-right = ##t}


\context  Voice { 
  << f''1 { s4 s4 \< s4\! \> s4\! } >>
}



