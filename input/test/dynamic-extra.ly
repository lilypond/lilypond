
\version "2.1.28"
% probably should be merged into refman.
\header{
    texidoc = "@cindex Dynamic Piu Forte
Più forte dynamics is produced using @code{\markup}. " }

piuf =	\markup {  \italic "pi\\`u" \dynamic "f" }

\score{
    \notes\relative c''{
	c-\piuf
	c
	c2\< c2\!
	
	c2\< c2\!
	}
\paper{raggedright = ##t}
    }


