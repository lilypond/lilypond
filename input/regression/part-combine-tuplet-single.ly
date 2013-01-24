
\header {
  texidoc = "Tuplets in combined parts only print one bracket."
  }

\paper { ragged-right = ##T }

\version "2.17.11"

\score {
  <<
    \new Staff {
      \partcombine
      \relative c'' {
        \tuplet 3/2 { d4 d d ~ } d2
      }
      \relative c'' {
        \tuplet 3/2 { b4 a g ~ } g2
      }
    }
  >>
}
