
% 	$Id: norsk.ly,v 1.4 1998/05/15 06:57:27 fred Exp $	

%{

 Common norwegian names for notes, including versions without the
 double s-es to save typing, as well as the traditional names with
 them not to confuse musicians.  "es" or "ess" means flat, "is" or
 "iss" means sharp.

 Otherwise, the main difference from the dutch names is the that the
 "b" is called "h" in norwegian, while the dutch "bes" is a norwegian
 "b".

 Staying within the (proud?) naming convention for norwegian language
 setup used in LaTeX, this file is called "norsk.ly" instead of
 "norwegian.ly", even if all other languages' files use the english
 name.

 This file is based on "dutch.ly".  Basically, I copied "dutch.ly",
 duplicated all lines containing at least one "is" or "es", changed
 every other "is" into an "iss" and every other "es" into an "ess",
 added "ass" et al, and made some b->h-related changes.  There may be
 bugs in here; please tell me (or send patches) if you find any.

                                  Arvid Gr=F8tting <arvidg@ifi.uio.no>

%}

\notenames {
	ceses 	= \musical_pitch { -1 0 -2 }
	cessess = \musical_pitch { -1 0 -2 }
	ces 	= \musical_pitch { -1 0 -1 }
	cess 	= \musical_pitch { -1 0 -1 }
	c 	= \musical_pitch { -1 0 0 }
	cis 	= \musical_pitch { -1 0 1 }
	ciss 	= \musical_pitch { -1 0 1 }
	cisis	= \musical_pitch { -1 0 2 }
	cississ = \musical_pitch { -1 0 2 }
	deses 	= \musical_pitch { -1 1 -2 }
	dessess = \musical_pitch { -1 1 -2 }
	des 	= \musical_pitch { -1 1 -1 }
	dess 	= \musical_pitch { -1 1 -1 }
	d 	= \musical_pitch { -1 1 0 }
	dis 	= \musical_pitch { -1 1 1 }
	diss 	= \musical_pitch { -1 1 1 }
	disis 	= \musical_pitch { -1 1 2 }
	dississ = \musical_pitch { -1 1 2 }
	eeses	= \musical_pitch { -1 2 -2 }
	eessess	= \musical_pitch { -1 2 -2 }
	eses 	= \musical_pitch { -1 2 -2 }
	essess 	= \musical_pitch { -1 2 -2 }
	ees	= \musical_pitch { -1 2 -1 }
	eess	= \musical_pitch { -1 2 -1 }
	es 	= \musical_pitch { -1 2 -1 }
	ess 	= \musical_pitch { -1 2 -1 }
	e 	= \musical_pitch { -1 2 0 }
	eis 	= \musical_pitch { -1 2 1 }
	eiss 	= \musical_pitch { -1 2 1 }
	eisis 	= \musical_pitch { -1 2 2 }
	eississ = \musical_pitch { -1 2 2 }
	feses 	= \musical_pitch { -1 3 -2 }
	fessess = \musical_pitch { -1 3 -2 }
	fes 	= \musical_pitch { -1 3 -1 }
	fess 	= \musical_pitch { -1 3 -1 }
	f 	= \musical_pitch { -1 3 0 }
	fis 	= \musical_pitch { -1 3 1 }
	fiss 	= \musical_pitch { -1 3 1 }
	fisis 	= \musical_pitch { -1 3 2 }
	fississ = \musical_pitch { -1 3 2 }
	geses 	= \musical_pitch { -1 4 -2 }
	gessess = \musical_pitch { -1 4 -2 }
	ges 	= \musical_pitch { -1 4 -1 }
	gess 	= \musical_pitch { -1 4 -1 }
	g 	= \musical_pitch { -1 4 0 }
	g 	= \musical_pitch { -1 4 0 }
	gis 	= \musical_pitch { -1 4 1 }
	giss 	= \musical_pitch { -1 4 1 }
	gisis 	= \musical_pitch { -1 4 2 }
	gississ = \musical_pitch { -1 4 2 }
	aeses 	= \musical_pitch { -1 5 -2 }
	aessess = \musical_pitch { -1 5 -2 }
	ases 	= \musical_pitch { -1 5 -2 }
	assess 	= \musical_pitch { -1 5 -2 }
	aes 	= \musical_pitch { -1 5 -1 }
	aess 	= \musical_pitch { -1 5 -1 }
	as 	= \musical_pitch { -1 5 -1 }
	ass 	= \musical_pitch { -1 5 -1 }
	a 	= \musical_pitch { -1 5 0 }
	ais 	= \musical_pitch { -1 5 1 }
	aiss 	= \musical_pitch { -1 5 1 }
	aisis 	= \musical_pitch { -1 5 2 }
	aississ = \musical_pitch { -1 5 2 }
	bes 	= \musical_pitch { -1 6 -2 }
	bess 	= \musical_pitch { -1 6 -2 }
	b 	= \musical_pitch { -1 6 -1 }
	b 	= \musical_pitch { -1 6 -1 }
	h 	= \musical_pitch { -1 6 0 }
	his 	= \musical_pitch { -1 6 1 }
	hiss 	= \musical_pitch { -1 6 1 }
	hisis 	= \musical_pitch { -1 6 2 }
	hississ = \musical_pitch { -1 6 2 }


	%
	% upper case: 1 octave lower, as usual.
	%


	Ceses 	= \musical_pitch { -2 0 -2 }
	Cessess = \musical_pitch { -2 0 -2 }
	Ces 	= \musical_pitch { -2 0 -1 }
	Cess 	= \musical_pitch { -2 0 -1 }
	C 	= \musical_pitch { -2 0 0 }
	Cis 	= \musical_pitch { -2 0 1 }
	Ciss 	= \musical_pitch { -2 0 1 }
	Cisis 	= \musical_pitch { -2 0 2 }
	Cississ = \musical_pitch { -2 0 2 }
	Deses 	= \musical_pitch { -2 1 -2 }
	Dessess = \musical_pitch { -2 1 -2 }
	Des 	= \musical_pitch { -2 1 -1 }
	Dess 	= \musical_pitch { -2 1 -1 }
	D 	= \musical_pitch { -2 1 0 }
	D 	= \musical_pitch { -2 1 0 }
	Dis 	= \musical_pitch { -2 1 1 }
	Diss 	= \musical_pitch { -2 1 1 }
	Disis 	= \musical_pitch { -2 1 2 }
	Dississ = \musical_pitch { -2 1 2 }
	Eses 	= \musical_pitch { -2 2 -2 }
	Essess 	= \musical_pitch { -2 2 -2 }
	Es 	= \musical_pitch { -2 2 -1 }
	Ess 	= \musical_pitch { -2 2 -1 }
	E 	= \musical_pitch { -2 2 0 }
	E 	= \musical_pitch { -2 2 0 }
	Eis 	= \musical_pitch { -2 2 1 }
	Eiss 	= \musical_pitch { -2 2 1 }
	Eisis 	= \musical_pitch { -2 2 2 }
	Eississ = \musical_pitch { -2 2 2 }
	Feses 	= \musical_pitch { -2 3 -2 }
	Fessess = \musical_pitch { -2 3 -2 }
	Fes 	= \musical_pitch { -2 3 -1 }
	Fess 	= \musical_pitch { -2 3 -1 }
	F 	= \musical_pitch { -2 3 0 }
	Fis 	= \musical_pitch { -2 3 1 }
	Fiss 	= \musical_pitch { -2 3 1 }
	Fisis 	= \musical_pitch { -2 3 2 }
	Fississ = \musical_pitch { -2 3 2 }
	Geses 	= \musical_pitch { -2 4 -2 }
	Gessess = \musical_pitch { -2 4 -2 }
	Ges 	= \musical_pitch { -2 4 -1 }
	Gess 	= \musical_pitch { -2 4 -1 }
	G 	= \musical_pitch { -2 4 0 }
	Gis 	= \musical_pitch { -2 4 1 }
	Giss 	= \musical_pitch { -2 4 1 }
	Gisis 	= \musical_pitch { -2 4 2 }
	Gississ = \musical_pitch { -2 4 2 }
	Aeses 	= \musical_pitch { -2 5 -2 }
	Aessess = \musical_pitch { -2 5 -2 }
	Ases 	= \musical_pitch { -2 5 -2 }
	Assess 	= \musical_pitch { -2 5 -2 }
	Aes 	= \musical_pitch { -2 5 -1 }
	Aess 	= \musical_pitch { -2 5 -1 }
	As 	= \musical_pitch { -2 5 -1 }
	Ass 	= \musical_pitch { -2 5 -1 }
	A 	= \musical_pitch { -2 5 0 }
	A 	= \musical_pitch { -2 5 0 }
	Ais 	= \musical_pitch { -2 5 1 }
	Aiss 	= \musical_pitch { -2 5 1 }
	Aisis 	= \musical_pitch { -2 5 2 }
	Aississ = \musical_pitch { -2 5 2 }
	Bes 	= \musical_pitch { -2 6 -2 }
	Bess 	= \musical_pitch { -2 6 -2 }
	B 	= \musical_pitch { -2 6 -1 }
	H 	= \musical_pitch { -2 6 0 }
	His 	= \musical_pitch { -2 6 1 }
	Hiss 	= \musical_pitch { -2 6 1 }
	Hisis 	= \musical_pitch { -2 6 2 }
	Hississ = \musical_pitch { -2 6 2 }

}



