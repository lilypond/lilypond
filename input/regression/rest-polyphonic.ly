\header {

  texidoc = "In polyphonic situations, rests are moved according
to their @code{direction} even if there is no opposite note or rest.
The amount is two @code{staff-space}s. "

}

\layout { ragged-right = ##t }


\version "2.14.0"


\new Staff  \relative c' {
  << { r8 g''4 g8       r g4 g8 } \\
     { d,4    r \stemUp b   r   } >>
}


