\header {
    
texidoc = "Blank music paper, another example: empty staffs and a
tablature staff."

}

emptymusic = \notes { \repeat unfold 4  { s1\break }  \bar "|." }

\score  {
\notes  <
	\context Staff \emptymusic
	\context TabStaff \emptymusic
	>


  \paper {
    \translator {
      \ScoreContext
      \remove Bar_number_engraver
    }
  }
}
