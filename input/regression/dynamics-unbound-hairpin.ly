\version "2.1.36"

\header {
texidoc = "Crescendi may start off-notes, however, they should  not collapse into flat lines."
}

\score { \notes { \context  Voice { 
  << f''1 { s4 s4 \< s4\! \> s4\! } >>
}}

\paper { raggedright = ##t}
}

