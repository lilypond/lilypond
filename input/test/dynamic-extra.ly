
\version "2.3.22"
% probably should be merged into refman.
\header{
    texidoc = "@cindex Dynamic Piu Forte
Più forte dynamics is produced using @code{\markup}. " }

piuf =	\markup {  \italic "pi\\`u" \dynamic "f" }

\score{
    \relative c''{
	c-\piuf
	c
	c2\< c2\!
	
	c2\< c2\!
	}
\layout{raggedright = ##t}
    }


