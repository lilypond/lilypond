\version "2.6.0"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with barlines.
"
}

\layout {
    raggedright = ##t
}

<<
    \context Staff = "foo" \with
    {
%	\remove "Bar_engraver"	
	}
    {
	b1 \bar "|:" b1 \bar ":|"
    }
    \context Lyrics \with {
	\consists "Bar_engraver"
	\override BarLine #'bar-size = #4 
    } \lyricmode {
	  looooooooooooooooooooooooooooooooooong1 syllable
      }
    \lyrics {
	no Bar_Engraver_Bar_Engraver_Bar_Engraver 
    }
    \context Staff = "bar" \with {
%		\remove "Bar_engraver"	
    } { b1 b1 }
>>
	

