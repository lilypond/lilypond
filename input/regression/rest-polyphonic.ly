\header {

    texidoc = "In polyphonic situations, rests are moved down
even if there is no opposite note or rest. The amount is two
staff-spaces. "

    }


\version "2.1.22"

\score {
  \new Staff \notes \relative c' {
     << { r8 g''4 g8 r g4 g8 } \\
            { d,4    r   d   r   } >>
  }

}
