\version "2.6.0"

\header {
texidoc = "Crescendi may start off-notes, however, they should  not collapse into flat lines."
}

\context  Voice { 
  << f''1 { s4 s4 \< s4\! \> s4\! } >>
}

\layout { raggedright = ##t}

