
\score { \context Staff \notes \relative c''  {
	[c8. c16]
	[c8. c16 c8.  c16]
	[c16 c8.] |
	[c8. c16  c16  c8.]
	[c8. c32  c32]
	[c8 c8] |
	[c16 c16]
	[c32 c32]
	[c64 c64]	
	c32
	[c8 c,4 c'8] % should warn here!
	[c8 c c] c8 % over barline
	[c16 c8 c16]
	[c32 c16 c16 c16 c32]
	[c32 c16 c8 c32]		 % hmm ?
	
		}}

