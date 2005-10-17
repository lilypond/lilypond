\header {

  texidoc = "In polyphonic situations, rests are moved down
even if there is no opposite note or rest. The amount is two
@code{staff-space}s. "

}

\layout { raggedright = ##t }


\version "2.7.13"


\new Staff  \relative c' {
  << { r8 g''4 g8 r g4 g8 } \\
     { d,4    r   d   r   } >>
}


