%
% scales with accents.
%

\version "0.0.56";
blah = \staff{  melodicregs
	\melodic {
		\meter 6/8;	
		\duration 4.;
		\octave  'c ;
	\clef "bass";
			c d
		\octave  c ;
			c d 
		\clef "violin" ;
		\octave  c' ;
		++ \transpose { d { e f } } ++
		\octave  c'' ;
			c d 
		\duration  8 ;
	%ugr
		 |[ a \< a a a a a a  \! a a \ff \> ]6/9 
		\octave  c' ;
		 |[ a  a a a a a a a \! a ]6/9 
		\octave  c'' ; 

		 ['c->-.-\fermata \< 'g->-.-\fermata d->-.-\fermata a->-.-\fermata
		e'->-.-\fermata b'-. f''-. c'''->-.-\fermata \! g'''->-.-\fermata \> ]6/9
		 [g'''->-.-\fermata c'''->-.-\fermata f''->-.-\fermata b'->-.-\fermata e'->-.-\fermata a->-.-\fermata d->-.-\fermata 'g->-.-\fermata \! 'c->-.-\fermata ]6/9
		 \octave c;
		['c->-.-\fermata \< 'g->-.-\fermata d->-.-\fermata a->-.-\fermata
		e'->-.-\fermata b'-. f''-. c'''->-.-\fermata \! g'''->-.-\fermata \> ]6/9
		 [g'''->-.-\fermata c'''->-.-\fermata f''->-.-\fermata b'->-.-\fermata e'->-.-\fermata a->-.-\fermata d->-.-\fermata 'g->-.-\fermata \! 'c->-.-\fermata ]6/9
		\octave  c' ;
		\octave  c' ;   
		 [ c g d' ]2/3 
		 [ d' g c ]2/3  
		 [ f c' g' ]2/3 
		 [ g' c' f ]2/3  
		\octave  c ;
		 [ c g d' ]2/3 
		 [ d' g c ]2/3  
		 [ f c' g' ]2/3 
		 [ g' c' f ]2/3 
		 [ g' c' f ]2/3 
  \meter 4/4;
	
c1
	\duration  8;
		r8-"text" r8^. r8_. r8 r8 r8 r8 r8
	[c-> d-> e-> f->][g-> a-> b-> c'->] % 1
\octave c';	[c'-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
\octave c;	[c'-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
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
}

\score{
	blah
	\paper{
		\symboltables {table_sixteen}
		\unitspace 1.5 \cm
		\geometric 1.4
	}
}
