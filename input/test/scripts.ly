	
\version "1.3.42";


blah = 	\notes{ \transpose c'' {

	c,4 ->-.-\fermata-"text"
	c ->-.-\fermata-"text"
	c' ->-.-\fermata-"text"
	c'' ->-.-\fermata-"text"

	d, ->-.-\fermata-"text"
	d ->-.-\fermata-"text"
	d' ->-.-\fermata-"text"
	d'' ->-.-\fermata-"text"

	c, ^>^.^\fermata^"text"
	c ^>^.^\fermata^"text"
	c' ^>^.^\fermata^"text"
	c'' ^>^.^\fermata^"text"

	d, ^>^.^\fermata^"text"
	d ^>^.^\fermata^"text"
	d' ^>^.^\fermata^"text"
	d'' ^>^.^\fermata ^"text"

	c, _>_._\fermata _"text"
	c _>_._\fermata _"text"
	c' _>_._\fermata _"text"
	c'' _>_._\fermata _"text"

	d, _>_._\fermata _"text"
	d _>_._\fermata _"text"
	d' _>_._\fermata _"text"
	d'' _>_._\fermata _"text"
			
	\stemup
	b,-. c-. d-. e-. f-. g-. a-. b-. c'-.
	\stemdown
	a-. b-. c'-. d'-. e'-. f'-. g'-. a'-. b'-.
	\stemup
	b,-> c-> d-> e-> f-> g-> a-> b-> c'->
	\stemup
	b,---. c---. d---. e---. f---. g---. a---. b---. c'---.
	\stemboth
      \time 4/4;
	[c8 \< d e \! f][d' \> e' f' \! g'] 
	[c-> \< d-> e-> \! f->][d'-> \> e'-> f'-> \! g'->] 
	[c-^ \< d-^ e-^ \! f-^][d'-^ \> e'-^ f'-^ \! g'-^] 
	[c-. \< d-. e-. \! f-.][d'-. \> e'-. f'-. \! g'-.]
	[c-- \< d-- e-- \! f--][d'-- \> e'-- f'-- \! g'--] 
	[c-\tenuto \< d-\tenuto e-\tenuto \! f-\tenuto]
		[d'-\tenuto \> e'-\tenuto f'-\tenuto \! g'-\tenuto]
	[c-\upbow \< d-\upbow e-\upbow \! f-\upbow]
		[d'-\upbow \> e'-\upbow f'-\upbow \! g'-\upbow] 
	[c-| \< d-| e-| \! f-|][d'-| \> e'-| f'-| \! g'-|]
	[c-\fermata \< d-\fermata e-\fermata \! f-\fermata]
		[d'-\fermata \> e'-\fermata f'-\fermata \! g'-\fermata] 
	[c-\lheel \< d-\lheel e-\lheel \! f-\lheel]
		[d'-\lheel \> e'-\lheel f'-\lheel \! g'-\lheel]

}}


\score{
	\blah

}
