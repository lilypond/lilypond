%
% This file prints lots of warning messages about scripts-dportato
%

\header{
filename =	 "scales.ly";
enteredby =	 "HWN";
copyright =	 "public domain";
}

%{
 Tested Features: scripts, beams, transposition, 
%}

%
% scales with accents.
%

\version "1.1.52";
blah = 	\notes {
		\time 6/8;	
\transpose c {
		\clef "bass";
			c4. d
}
			c d 
		\clef "violin" ;
\transpose c'' {
		\transpose  d { e f }  }
\transpose c''' {
		c4. d 
	%ugr
		 |\times 6/9 { [ a8 \< a a a a a a  \! a a \ff \> ] } | }
		 \transpose c'' {
		     \times 6/9 { [ a  a a a a a a a \! a ] } | }
		 \transpose c''' {
		 \times 6/9 { [ c,->-.-\fermata \< g,->-.-\fermata 
			d->-.-\fermata a->-.-\fermata
			e'->-.-\fermata b'-. f''-. c'''->-.-\fermata 
			\! g'''->-.-\fermata \> ] }

		\times 6/9 { [ g'''->-.-\fermata c'''->-.-\fermata
			f''->-.-\fermata b'->-.-\fermata e'->-.-\fermata 
			a->-.-\fermata d->-.-\fermata g,->-.-\fermata \! 
			c,->-.-\fermata ] }
			
		\times 6/9 { [ c,->-.-\fermata \< g,->-.-\fermata d->-.-\fermata 
		a->-.-\fermata
			e'->-.-\fermata b'-. f''-. 
			c'''->-.-\fermata \! g'''->-.-\fermata \> ] }
		 \times 6/9 { [ g'''->-.-\fermata c'''->-.-\fermata
			f''->-.-\fermata b'->-.-\fermata e'->-.-\fermata 
			a->-.-\fermata d->-.-\fermata 
			g,->-.-\fermata \! c,->-.-\fermata ] }
		}
		\transpose c'' {
		 \times 2/3 { [ c g d' ] } 
		 \times 2/3 { [ d' g c ] }  
		 \times 2/3 { [ f c' g' ] } 
		 \times 2/3 { [ g' c' f ] }
		 }
		 \transpose c' {
		 \times 2/3 { [ c g d' ] } 
		 \times 2/3 { [ d' g c ] }  
		 \times 2/3 { [ f c' g' ] } 
		 \times 2/3 { [ g' c' f ] } 
		 \times 2/3 { [ g' c' f ] } 
  \time 4/4;

c1}

		r8-"text" r8^. r8_. r8 r8 r8 r8 r8
	[c-> d-> e-> f->][g-> a-> b-> c'->] % 1
\transpose c'' {	[c'-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]}
	[c'-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
	[c-. d-. e-. f-.][g-. a-. b-. c'-.]
	[c'-- b-- a-- g][f-- e-- d c--] % 5
	[c-\portato d-\portato e-\portato f-\portato]
		[g-\portato a-\portato b-\portato c'-\portato]
	[c'-\upbow b-\upbow a-\downbow g-\downbow]
		[f-\downbow e-\downbow d-\upbow c-\upbow]
	[c-| d-| e-| f-|][g-| a-| b-| c'-|]
	[c' b a g][f e d c]
	[c d e f][g a b c'] % 10 
	|[c' b a g][f e d c]
			
	}


\score{
	\notes {\blah}
	\paper{
	 castingalgorithm = \Wordwrap;
	}
}
