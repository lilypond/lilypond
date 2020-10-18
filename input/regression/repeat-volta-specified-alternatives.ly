\version "2.23.0"
\header {
  texidoc = "@code{\\volta} assigns bracket labels without reordering
alternatives.  A final alternative that is not exclusive to the final
volta ends with a repeat bar."
}

\paper { ragged-right = ##t }

musicA = \fixed c'' {
  \repeat volta 3 c1
  \alternative {
    \volta 1 d
    \volta 3,2 f
  }
  e4
}

musicB = \fixed c'' {
  \repeat volta 3 c1
  \alternative {
    \volta 3,2 f
    \volta 1 d
  }
  e4
}

\score { \musicA }
\score { \musicB }
