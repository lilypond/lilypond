\version "2.3.4"
\header {
    
texidoc = "@cindex Blank Paper Tab
A blank music paper can be produced by using spacer rests, and removing
@code{Bar_number_engraver}. Here is an empty staff with a tablature staff. 
" }

emptymusic =  { \repeat unfold 4  { s1\break }  \bar "|." }

\score  {
  <<
	\context Staff \emptymusic
	\context TabStaff \emptymusic
	>>


  \paper {
    \context {
      \Score
      \remove Bar_number_engraver
    }
  }
}

