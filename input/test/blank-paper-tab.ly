\version "2.1.26"
\header {
    
texidoc = "@cindex Blank Paper Tab
A blank music paper can be produced by using spacer rests, and removing
@code{Bar_number_engraver}. Here is an empty staff with a tablature staff. 
" }

emptymusic = \notes { \repeat unfold 4  { s1\break }  \bar "|." }

\score  {
\notes  <<
	\context Staff \emptymusic
	\context TabStaff \emptymusic
	>>


  \paper {
    \translator {
      \ScoreContext
      \remove Bar_number_engraver
    }
  }
}

