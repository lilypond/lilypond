#(ly:set-option 'old-relative)
\version "1.9.0"
% probably should be merged into refman.
\header{
    texidoc = "@cindex Dynamic Piu Forte
Additional tricks for dynamics.  Pi`u forte dynamic script. " }

piuf =	\markup {  \italic "pi\\`u" \dynamic "f" }

\score{
    \notes\relative c''{
	c-\piuf
	c
	c2\< c2-\!
	
	c2\< c2-\!
	}
\paper{raggedright = ##t}
    }


