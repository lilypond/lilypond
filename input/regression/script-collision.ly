\version "2.12.0"

\header {

  texidoc = "Scripts are put on the utmost head, so they are
      positioned correctly when there are collisions."
}

\layout { ragged-right = ##t}


\relative c'' {
  c4
  <c d c'>\marcato
  <<  { c4^^ }\\
      { d4_^ } >>
}



