
% 	$Id: norsk.ly,v 1.3 1998/03/26 11:07:44 fred Exp $	

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
	ceses 	= \melodic_request { -1 0 -2 }
	cessess = \melodic_request { -1 0 -2 }
	ces 	= \melodic_request { -1 0 -1 }
	cess 	= \melodic_request { -1 0 -1 }
	c 	= \melodic_request { -1 0 0 }
	cis 	= \melodic_request { -1 0 1 }
	ciss 	= \melodic_request { -1 0 1 }
	cisis	= \melodic_request { -1 0 2 }
	cississ = \melodic_request { -1 0 2 }
	deses 	= \melodic_request { -1 1 -2 }
	dessess = \melodic_request { -1 1 -2 }
	des 	= \melodic_request { -1 1 -1 }
	dess 	= \melodic_request { -1 1 -1 }
	d 	= \melodic_request { -1 1 0 }
	dis 	= \melodic_request { -1 1 1 }
	diss 	= \melodic_request { -1 1 1 }
	disis 	= \melodic_request { -1 1 2 }
	dississ = \melodic_request { -1 1 2 }
	eeses	= \melodic_request { -1 2 -2 }
	eessess	= \melodic_request { -1 2 -2 }
	eses 	= \melodic_request { -1 2 -2 }
	essess 	= \melodic_request { -1 2 -2 }
	ees	= \melodic_request { -1 2 -1 }
	eess	= \melodic_request { -1 2 -1 }
	es 	= \melodic_request { -1 2 -1 }
	ess 	= \melodic_request { -1 2 -1 }
	e 	= \melodic_request { -1 2 0 }
	eis 	= \melodic_request { -1 2 1 }
	eiss 	= \melodic_request { -1 2 1 }
	eisis 	= \melodic_request { -1 2 2 }
	eississ = \melodic_request { -1 2 2 }
	feses 	= \melodic_request { -1 3 -2 }
	fessess = \melodic_request { -1 3 -2 }
	fes 	= \melodic_request { -1 3 -1 }
	fess 	= \melodic_request { -1 3 -1 }
	f 	= \melodic_request { -1 3 0 }
	fis 	= \melodic_request { -1 3 1 }
	fiss 	= \melodic_request { -1 3 1 }
	fisis 	= \melodic_request { -1 3 2 }
	fississ = \melodic_request { -1 3 2 }
	geses 	= \melodic_request { -1 4 -2 }
	gessess = \melodic_request { -1 4 -2 }
	ges 	= \melodic_request { -1 4 -1 }
	gess 	= \melodic_request { -1 4 -1 }
	g 	= \melodic_request { -1 4 0 }
	g 	= \melodic_request { -1 4 0 }
	gis 	= \melodic_request { -1 4 1 }
	giss 	= \melodic_request { -1 4 1 }
	gisis 	= \melodic_request { -1 4 2 }
	gississ = \melodic_request { -1 4 2 }
	aeses 	= \melodic_request { -1 5 -2 }
	aessess = \melodic_request { -1 5 -2 }
	ases 	= \melodic_request { -1 5 -2 }
	assess 	= \melodic_request { -1 5 -2 }
	aes 	= \melodic_request { -1 5 -1 }
	aess 	= \melodic_request { -1 5 -1 }
	as 	= \melodic_request { -1 5 -1 }
	ass 	= \melodic_request { -1 5 -1 }
	a 	= \melodic_request { -1 5 0 }
	ais 	= \melodic_request { -1 5 1 }
	aiss 	= \melodic_request { -1 5 1 }
	aisis 	= \melodic_request { -1 5 2 }
	aississ = \melodic_request { -1 5 2 }
	bes 	= \melodic_request { -1 6 -2 }
	bess 	= \melodic_request { -1 6 -2 }
	b 	= \melodic_request { -1 6 -1 }
	b 	= \melodic_request { -1 6 -1 }
	h 	= \melodic_request { -1 6 0 }
	his 	= \melodic_request { -1 6 1 }
	hiss 	= \melodic_request { -1 6 1 }
	hisis 	= \melodic_request { -1 6 2 }
	hississ = \melodic_request { -1 6 2 }


	%
	% upper case: 1 octave lower, as usual.
	%


	Ceses 	= \melodic_request { -2 0 -2 }
	Cessess = \melodic_request { -2 0 -2 }
	Ces 	= \melodic_request { -2 0 -1 }
	Cess 	= \melodic_request { -2 0 -1 }
	C 	= \melodic_request { -2 0 0 }
	Cis 	= \melodic_request { -2 0 1 }
	Ciss 	= \melodic_request { -2 0 1 }
	Cisis 	= \melodic_request { -2 0 2 }
	Cississ = \melodic_request { -2 0 2 }
	Deses 	= \melodic_request { -2 1 -2 }
	Dessess = \melodic_request { -2 1 -2 }
	Des 	= \melodic_request { -2 1 -1 }
	Dess 	= \melodic_request { -2 1 -1 }
	D 	= \melodic_request { -2 1 0 }
	D 	= \melodic_request { -2 1 0 }
	Dis 	= \melodic_request { -2 1 1 }
	Diss 	= \melodic_request { -2 1 1 }
	Disis 	= \melodic_request { -2 1 2 }
	Dississ = \melodic_request { -2 1 2 }
	Eses 	= \melodic_request { -2 2 -2 }
	Essess 	= \melodic_request { -2 2 -2 }
	Es 	= \melodic_request { -2 2 -1 }
	Ess 	= \melodic_request { -2 2 -1 }
	E 	= \melodic_request { -2 2 0 }
	E 	= \melodic_request { -2 2 0 }
	Eis 	= \melodic_request { -2 2 1 }
	Eiss 	= \melodic_request { -2 2 1 }
	Eisis 	= \melodic_request { -2 2 2 }
	Eississ = \melodic_request { -2 2 2 }
	Feses 	= \melodic_request { -2 3 -2 }
	Fessess = \melodic_request { -2 3 -2 }
	Fes 	= \melodic_request { -2 3 -1 }
	Fess 	= \melodic_request { -2 3 -1 }
	F 	= \melodic_request { -2 3 0 }
	Fis 	= \melodic_request { -2 3 1 }
	Fiss 	= \melodic_request { -2 3 1 }
	Fisis 	= \melodic_request { -2 3 2 }
	Fississ = \melodic_request { -2 3 2 }
	Geses 	= \melodic_request { -2 4 -2 }
	Gessess = \melodic_request { -2 4 -2 }
	Ges 	= \melodic_request { -2 4 -1 }
	Gess 	= \melodic_request { -2 4 -1 }
	G 	= \melodic_request { -2 4 0 }
	Gis 	= \melodic_request { -2 4 1 }
	Giss 	= \melodic_request { -2 4 1 }
	Gisis 	= \melodic_request { -2 4 2 }
	Gississ = \melodic_request { -2 4 2 }
	Aeses 	= \melodic_request { -2 5 -2 }
	Aessess = \melodic_request { -2 5 -2 }
	Ases 	= \melodic_request { -2 5 -2 }
	Assess 	= \melodic_request { -2 5 -2 }
	Aes 	= \melodic_request { -2 5 -1 }
	Aess 	= \melodic_request { -2 5 -1 }
	As 	= \melodic_request { -2 5 -1 }
	Ass 	= \melodic_request { -2 5 -1 }
	A 	= \melodic_request { -2 5 0 }
	A 	= \melodic_request { -2 5 0 }
	Ais 	= \melodic_request { -2 5 1 }
	Aiss 	= \melodic_request { -2 5 1 }
	Aisis 	= \melodic_request { -2 5 2 }
	Aississ = \melodic_request { -2 5 2 }
	Bes 	= \melodic_request { -2 6 -2 }
	Bess 	= \melodic_request { -2 6 -2 }
	B 	= \melodic_request { -2 6 -1 }
	H 	= \melodic_request { -2 6 0 }
	His 	= \melodic_request { -2 6 1 }
	Hiss 	= \melodic_request { -2 6 1 }
	Hisis 	= \melodic_request { -2 6 2 }
	Hississ = \melodic_request { -2 6 2 }

}



