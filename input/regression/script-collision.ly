\version "2.1.30"

\header {

    texidoc = "Scripts are put on the utmost head, so they are
      positioned correctly when there are collisions."
}

\score  {
\notes \relative c'' {
  c4
  <c d c'>\marcato
  <<  { c4^^ }\\
     { d4_^ } >>
    }
\paper { raggedright = ##t}
    }

