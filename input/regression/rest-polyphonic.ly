\header {

    texidoc = "In polyphonic situations, rests are moved down
even if there is no opposite note or rest. The amount is two
@code{staff-space}s. "

    }


\version "2.4.0"

\score {
  \new Staff  \relative c' {
     << { r8 g''4 g8 r g4 g8 } \\
            { d,4    r   d   r   } >>
  }
  \layout { raggedright = ##t }
}
