
\header {

  texidoc = "In rest-note collisions, the rest moves in discrete
  steps, and inside the staff, it moves in whole staff spaces."

	}


\version "2.12.0"
\new Staff {

  <<
    \relative c'' {
      f e d c b a g f e d c
    }
    \\
    {
      r4 r r r r r r r r r r
    }
  >>
  <<
    {
      r4 r r r r r r r r r r
    }
    \\
    \relative c'' {
      f e d c b a g f e d c
    }
  >>
}
