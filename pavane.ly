% pavane pour une Infante d\'efunte
% 
% Maurice Ravel
%
% (Ravel has been dead for over 50 years. This does not have copyright)
%

horn =
staff {melodic
	music{ 	$
	\octave { ' }
	\duration { 8}

% 1
	d2( [)d e cis `b]		`a4 [`b cis] [cis `b] `b4
	fis2( [)fis g e d] 		cis4 [d e(] [)e fis d cis]
	`b4 [cis d(] [)d e cis `b] 	cis2 r2
	r4 fis2 fis4 			fis2()[fis e a fis]
	fis4 e4 d4 e4 			`b2()[`b `a( d cis]
% 11
	)`b [`fis `a `b] cis4 `b4	`fis2 r2
	cis4 d4()[d cis d e]
	\octave { '` }
					a4 gis2.
	a4 b4()[b a b 'cis]		fis4 e4 cis2
	e4 fis4 () [fis e fis gis]	cis4 `b4()`b8 r8 r4

	r4 r4 				'cis4 'd4 () ['d 'cis 'd 'e]
	a4 gis2.			a4 b4()[b a b 'cis]
	fis4 e4 cis2			e4 fis4()[fis e fis gis]
	cis4 `b4()`b8 r8 r4		r1
	r2 r4 r4			
%% cut 'n paste.
	\octave { ' }
	d2( [)d e cis `b]		`a4 [`b cis] [cis `b] `b4
	fis2( [)fis g e d] 		cis4 [d e(] [)e fis d cis]
	`b4 [cis d(] [)d e cis `b] 	cis2 r2
	r4 fis2 fis4 			fis2()[fis e a fis]
	fis4 e4 d4 e4 			`b2() [`b `a d cis]
	`b [`fis `a `b] cis4 `b4	`fis2 r2
	$}
	commands {	
		key  $fis cis $
	}
}

score {
	staff {
		horn
	}
	paper {
		symboltables { table_sixteen}
		unitspace 1.5 cm
		geometric 1.4
	}
	commands { meter 4 4
		skip 18:0
		meter 2 4
		skip 1:0
		meter 4 4 
		skip 20:0
	}
}