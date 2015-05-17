
\header {

  texidoc = "In rest-note collisions, the rest moves in discrete
  steps, and inside the staff, it moves in whole staff spaces."

	}


\version "2.19.21"

music = {
  <<
    \relative {
      f'' e d c b a g f e d c
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
    \relative {
      f'' e d c b a g f e d c
    }
  >>
}

\score {
  \new Staff { \music }
}

\score {
  \new Staff { \override Staff.Rest.style = #'z \music }
}

\score {
  \new Staff { \override Staff.Rest.style = #'classical \music }
}
