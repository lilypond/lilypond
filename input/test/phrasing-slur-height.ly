\header{
    texidoc="Make PhrasingSlur higher, to avoid colission from other slurs"
    }

\version "1.5.72"

\score {
    \context Staff = melody \notes\relative c''{
	\property Staff.PhrasingSlur \override #'height-limit = #8.0
	c8 \( (d e ) f g ( a b ) c
	| c ( b a ) g f ( e d )\) c
    }
    \paper {
    }
}
