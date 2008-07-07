\version "2.11.51"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with barlines.
"
}

\layout {
    ragged-right = ##t
}

<<
    \new Staff \with
    {
%	\remove "Bar_engraver"	
	}
    {
	b1 \bar "|:" b1 \bar ":|"
    }
    \context Lyrics \with {
	\consists "Bar_engraver"
	\consists "Separating_line_group_engraver"
 	\override BarLine #'bar-size = #4 
    } \lyricmode {
	  looooooooooooooooooooooooooooooooooong1 syllable
      }
    \lyrics {
	no Bar_Engraver_Bar_Engraver_Bar_Engraver 
    }
    \new Staff \with {
%		\remove "Bar_engraver"	
    } { b1 b1 }
>>
	

