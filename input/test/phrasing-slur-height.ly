
\version "2.3.4"
\header{ texidoc="@cindex Phrasing Slur Height
The @code{PhrasingSlur} can be made higher in order to avoid collision 
with other slurs. "
}

\score {
    \new Staff \relative c''{
	\override Staff.PhrasingSlur  #'height-limit = #8.0
	c8 \( (d e f) g ( a b c)
	| c ( b a g) f ( e d c)\)
    }
    \paper { raggedright = ##t }
}

