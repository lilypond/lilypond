\version "2.23.12"

\header {
  texidoc = "A @code{\\layout} block inside a @code{\\paper}
block does not error out, and the variables from @code{\\paper}
are accessible in @code{\\layout}."
}

\book {
  \paper {
    head-color = "red"
    oddHeaderMarkup =
      \markup \fill-line {
        \score {
          \layout {
            \context {
              \Staff
              \magnifyStaff #0.5
              \override NoteHead.color = \head-color
            }
          }
          { e'8 e'8 d'8 d'8 }
        }
      }
  }
  { c'1 }
}
