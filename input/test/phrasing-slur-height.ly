
\version "2.1.26"
\header{ texidoc="@cindex Phrasing Slur Height
Make PhrasingSlur higher, to avoid colission from other slurs. "
}

\score {
    \new Staff \notes\relative c''{
	\override Staff.PhrasingSlur  #'height-limit = #8.0
	c8 \( (d e f) g ( a b c)
	| c ( b a g) f ( e d c)\)
    }
    \paper { raggedright = ##t }
}

