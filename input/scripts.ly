	
\version "0.1.9";


blah = 	\melodic {
	\octave c';
	'c ->-.-\fermata-"text"
	c ->-.-\fermata-"text"
	c' ->-.-\fermata-"text"
	c'' ->-.-\fermata-"text"

	'd ->-.-\fermata-"text"
	d ->-.-\fermata-"text"
	d' ->-.-\fermata-"text"
	d'' ->-.-\fermata-"text"

	'c ^>^.^\fermata^"text"
	c ^>^.^\fermata^"text"
	c' ^>^.^\fermata^"text"
	c'' ^>^.^\fermata^"text"

	'd ^>^.^\fermata^"text"
	d ^>^.^\fermata^"text"
	d' ^>^.^\fermata^"text"
	d'' ^>^.^\fermata ^"text"

	'c _>_._\fermata _"text"
	c _>_._\fermata _"text"
	c' _>_._\fermata _"text"
	c'' _>_._\fermata _"text"

	'd _>_._\fermata _"text"
	d _>_._\fermata _"text"
	d' _>_._\fermata _"text"
	d'' _>_._\fermata _"text"
			
      \meter 4/4;
	\duration  8;
	\octave c';
	[c \< d e \! f][d' \> e' f' \! g'] 
	[c-> \< d-> e-> \! f->][d'-> \> e'-> f'-> \! g'->] 
	[c-^ \< d-^ e-^ \! f-^][d'-^ \> e'-^ f'-^ \! g'-^] 
	[c-. \< d-. e-. \! f-.][d'-. \> e'-. f'-. \! g'-.]
	[c-- \< d-- e-- \! f--][d'-- \> e'-- f'-- \! g'--] 
	[c-\portato \< d-\portato e-\portato \! f-\portato]
		[d'-\portato \> e'-\portato f'-\portato \! g'-\portato]
	[c-\upbow \< d-\upbow e-\upbow \! f-\upbow]
		[d'-\upbow \> e'-\upbow f'-\upbow \! g'-\upbow] 
	[c-| \< d-| e-| \! f-|][d'-| \> e'-| f'-| \! g'-|]
	[c-\fermata \< d-\fermata e-\fermata \! f-\fermata]
		[d'-\fermata \> e'-\fermata f'-\fermata \! g'-\fermata] 
	[c-\lheel \< d-\lheel e-\lheel \! f-\lheel]
		[d'-\lheel \> e'-\lheel f'-\lheel \! g'-\lheel]
}


\score{
	\blah

}
