
\version "2.3.16"
\header
{
    texidoc ="Slurs may be placed over rest. The slur will avoid colliding with
the rest.

"
}
\score{   { \stemDown c'4 ( r4  c'2)

		  \relative c'' { 
	% Finish with F if played separately 
	c8-.(   r c,-. r  c4) r4|  }
		  

	      }
\paper { raggedright = ##t}
    }

