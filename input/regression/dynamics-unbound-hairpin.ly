\version "2.3.4"

\header {
texidoc = "Crescendi may start off-notes, however, they should  not collapse into flat lines."
}

\score {  { \context  Voice { 
  << f''1 { s4 s4 \< s4\! \> s4\! } >>
}}

\paper { raggedright = ##t}
}

