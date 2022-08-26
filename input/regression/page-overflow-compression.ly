\version "2.25.0"

\header {
  texidoc = "
Layouts that overflow a page will be compressed in order to fit on
the page, even if it causes collisions.  In this example, the
tagline should not collide with the bottom staff.
"
}

\paper {
  paper-height= 9\cm
}

\book {
  \repeat unfold 3 { g'''1\textMark "Long Text" g'''1\break}
}
