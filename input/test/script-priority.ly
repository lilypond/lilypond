
\version "1.9.1"

\header { texidoc = "@cindex Script Priority
Relative placements of different script types can be controlled
by overriding script-priority. "
}


\score{
    \context Staff \notes \relative g''{
	
 	\property Score.TextScript \override #'script-priority = #-100
	a4^\prall^\markup \fontsize #-2 \musicglyph #"accidentals-1"

	
 	\property Score.Script \override #'script-priority = #-100
 	\property Score.TextScript \revert #'script-priority
	
	a4^\prall^\markup \fontsize  #-2 \musicglyph #"accidentals-1"
    }
	\paper { raggedright = ##t} 
}

