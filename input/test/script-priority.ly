
\version "2.1.30"

\header { texidoc = "@cindex Script Priority
Relative placements of different script types can be controlled
by overriding @code{script-priority}. "
}


\score{
    \context Staff \notes \relative g''{
	
 	\override Score.TextScript  #'script-priority = #-100
	a4^\prall^\markup \fontsize #-2 \semisharp

	
 	\override Score.Script  #'script-priority = #-100
 	\revert Score.TextScript #'script-priority
	
	a4^\prall^\markup \fontsize  #-2 \semisharp
    }
	\paper { raggedright = ##t} 
}

