\version "2.3.4"

\header {

    texidoc = "Scripts are put on the utmost head, so they are
      positioned correctly when there are collisions."
}

\score  {
 \relative c'' {
  c4
  <c d c'>\marcato
  <<  { c4^^ }\\
     { d4_^ } >>
    }
\paper { raggedright = ##t}
    }

