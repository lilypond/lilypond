\version "1.0.7";

% urg, the direction of the slur gets swapped!
\score{
	\notes\relative c{
	\slurup
	\stemup
		[e'8( c' a b]
	\stemdown
		[e c a )e] \break 
	\stemup
		[e8( c' a b]
	\stemdown
		[e c a )e] e1
	}
	\paper{
		indent = 0.\mm
      		linewidth=80.\mm
	}
}
