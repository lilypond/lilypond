\header {

  texidoc = "In polyphonic situations, rests are moved according
to their @code{direction} even if there is no opposite note or rest.
The amount in @code{staff-position}s is set by @code{voiced-position}."

}

\layout { ragged-right = ##t }


\version "2.19.21"


\new Staff  \relative {
  << { r8 g''4 g8       r g4 g8 } \\
     { d,4    r \stemUp b   r   } >>
 \override Staff.Rest.voiced-position = 2
  << { r8 g''4 g8       r g4 g8 } \\
     { d,4    r \stemUp b   r   } >>
}


