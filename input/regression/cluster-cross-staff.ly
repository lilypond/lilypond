\header {

    texidoc = "Clusters can be written across staves."

}

\version "2.6.0"

\score {
    \new PianoStaff <<
    \context Staff = "up"     {
	s1 *2
	}
	\context Staff = "down" <<
	    \applymusic #notes-to-clusters \relative c  { <c e>4 <f a> <b e> \change Staff = up <e a>
						     <a d> <d g> }

	    { \clef bass s1 * 2 }
	    >>
>>
    \layout {
	raggedright= ##t
    }
}
