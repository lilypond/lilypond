\version "1.0.10";

% bug
% excentric slur can't handle this ...
\score{
	\notes{
		\stemdown 
		\times 4/5 {c8( c f,, c c} c c c )c |
	}
}
