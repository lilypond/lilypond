\header {

    texidoc = "Notes in different staves should be aligned to the
 left-most note, in case of collisions."

}

\layout  { ragged-right = ##t }

\version "2.11.51"

\relative <<
    \new Staff {
	<<
	    { f g } \\
	    { g f }
	>> }
    \new Staff {
	c4 c 
    }
>>
