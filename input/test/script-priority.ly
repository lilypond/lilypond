
\version "2.3.8"

\header { texidoc = "@cindex Script Priority
Relative placements of different script types can be controlled
by overriding @code{script-priority}.

In this example, accidentals are put either below or above other
script symbols.

"
}


\score{
    \context Staff  \relative g''{
	
 	\override Score.TextScript  #'script-priority = #-100
	a4^\prall^\markup { \sharp }

	
 	\override Score.Script  #'script-priority = #-100
 	\revert Score.TextScript #'script-priority
	
	a4^\prall^\markup { \sharp }
    }
	\paper { raggedright = ##t} 
}

