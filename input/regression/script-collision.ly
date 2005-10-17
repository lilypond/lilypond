\version "2.7.13"

\header {

  texidoc = "Scripts are put on the utmost head, so they are
      positioned correctly when there are collisions."
}

\layout { raggedright = ##t}


\relative c'' {
  c4
  <c d c'>\marcato
  <<  { c4^^ }\\
      { d4_^ } >>
}



