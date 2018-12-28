
\header {
  texidoc = "Tuplets in combined parts only print one bracket."
  }

\paper { ragged-right = ##T }

\version "2.21.0"

\score {
  <<
    \new Staff {
      \partCombine
      \relative {
        \tuplet 3/2 { d''4 d d ~ } d2
      }
      \relative {
        \tuplet 3/2 { b'4 a g ~ } g2
      }
    }
  >>
}
