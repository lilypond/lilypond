\header {

  texidoc = "in collisions, the dots of outer voices avoid
  stems and flags of the inner voices."

}

\version "2.14.0"

\layout { ragged-right = ##t }

\new Staff {
  \key e \major \time 3/4
  \relative c'' {
    << { dis4.  } \\
       { fis,4 } \\ { b8 } >>
  }
}
