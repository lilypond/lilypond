\score{
	\context Staff \notes\relative c'' {
	% doesn't quite work yet
	%	\property Staff.unfoldRepeats = 1
		\repeat 3 { a b c d } 
		\alternative { { d c b a } { a b c d } } d d d d
	}
}

\version "1.0.16"; 
