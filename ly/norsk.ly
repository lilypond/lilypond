
% 	$Id: norsk.ly,v 1.2 1999/09/02 00:17:49 fred Exp $	

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
	ceses 	= \musicalpitch { -1 0 -2 }
	cessess = \musicalpitch { -1 0 -2 }
	ces 	= \musicalpitch { -1 0 -1 }
	cess 	= \musicalpitch { -1 0 -1 }
	c 	= \musicalpitch { -1 0 0 }
	cis 	= \musicalpitch { -1 0 1 }
	ciss 	= \musicalpitch { -1 0 1 }
	cisis	= \musicalpitch { -1 0 2 }
	cississ = \musicalpitch { -1 0 2 }
	deses 	= \musicalpitch { -1 1 -2 }
	dessess = \musicalpitch { -1 1 -2 }
	des 	= \musicalpitch { -1 1 -1 }
	dess 	= \musicalpitch { -1 1 -1 }
	d 	= \musicalpitch { -1 1 0 }
	dis 	= \musicalpitch { -1 1 1 }
	diss 	= \musicalpitch { -1 1 1 }
	disis 	= \musicalpitch { -1 1 2 }
	dississ = \musicalpitch { -1 1 2 }
	eeses	= \musicalpitch { -1 2 -2 }
	eessess	= \musicalpitch { -1 2 -2 }
	eses 	= \musicalpitch { -1 2 -2 }
	essess 	= \musicalpitch { -1 2 -2 }
	ees	= \musicalpitch { -1 2 -1 }
	eess	= \musicalpitch { -1 2 -1 }
	es 	= \musicalpitch { -1 2 -1 }
	ess 	= \musicalpitch { -1 2 -1 }
	e 	= \musicalpitch { -1 2 0 }
	eis 	= \musicalpitch { -1 2 1 }
	eiss 	= \musicalpitch { -1 2 1 }
	eisis 	= \musicalpitch { -1 2 2 }
	eississ = \musicalpitch { -1 2 2 }
	feses 	= \musicalpitch { -1 3 -2 }
	fessess = \musicalpitch { -1 3 -2 }
	fes 	= \musicalpitch { -1 3 -1 }
	fess 	= \musicalpitch { -1 3 -1 }
	f 	= \musicalpitch { -1 3 0 }
	fis 	= \musicalpitch { -1 3 1 }
	fiss 	= \musicalpitch { -1 3 1 }
	fisis 	= \musicalpitch { -1 3 2 }
	fississ = \musicalpitch { -1 3 2 }
	geses 	= \musicalpitch { -1 4 -2 }
	gessess = \musicalpitch { -1 4 -2 }
	ges 	= \musicalpitch { -1 4 -1 }
	gess 	= \musicalpitch { -1 4 -1 }
	g 	= \musicalpitch { -1 4 0 }
	g 	= \musicalpitch { -1 4 0 }
	gis 	= \musicalpitch { -1 4 1 }
	giss 	= \musicalpitch { -1 4 1 }
	gisis 	= \musicalpitch { -1 4 2 }
	gississ = \musicalpitch { -1 4 2 }
	aeses 	= \musicalpitch { -1 5 -2 }
	aessess = \musicalpitch { -1 5 -2 }
	ases 	= \musicalpitch { -1 5 -2 }
	assess 	= \musicalpitch { -1 5 -2 }
	aes 	= \musicalpitch { -1 5 -1 }
	aess 	= \musicalpitch { -1 5 -1 }
	as 	= \musicalpitch { -1 5 -1 }
	ass 	= \musicalpitch { -1 5 -1 }
	a 	= \musicalpitch { -1 5 0 }
	ais 	= \musicalpitch { -1 5 1 }
	aiss 	= \musicalpitch { -1 5 1 }
	aisis 	= \musicalpitch { -1 5 2 }
	aississ = \musicalpitch { -1 5 2 }
	bes 	= \musicalpitch { -1 6 -2 }
	bess 	= \musicalpitch { -1 6 -2 }
	b 	= \musicalpitch { -1 6 -1 }
	b 	= \musicalpitch { -1 6 -1 }
	h 	= \musicalpitch { -1 6 0 }
	his 	= \musicalpitch { -1 6 1 }
	hiss 	= \musicalpitch { -1 6 1 }
	hisis 	= \musicalpitch { -1 6 2 }
	hississ = \musicalpitch { -1 6 2 }


	%
	% upper case: 1 octave lower, as usual.
	%


	Ceses 	= \musicalpitch { -2 0 -2 }
	Cessess = \musicalpitch { -2 0 -2 }
	Ces 	= \musicalpitch { -2 0 -1 }
	Cess 	= \musicalpitch { -2 0 -1 }
	C 	= \musicalpitch { -2 0 0 }
	Cis 	= \musicalpitch { -2 0 1 }
	Ciss 	= \musicalpitch { -2 0 1 }
	Cisis 	= \musicalpitch { -2 0 2 }
	Cississ = \musicalpitch { -2 0 2 }
	Deses 	= \musicalpitch { -2 1 -2 }
	Dessess = \musicalpitch { -2 1 -2 }
	Des 	= \musicalpitch { -2 1 -1 }
	Dess 	= \musicalpitch { -2 1 -1 }
	D 	= \musicalpitch { -2 1 0 }
	D 	= \musicalpitch { -2 1 0 }
	Dis 	= \musicalpitch { -2 1 1 }
	Diss 	= \musicalpitch { -2 1 1 }
	Disis 	= \musicalpitch { -2 1 2 }
	Dississ = \musicalpitch { -2 1 2 }
	Eses 	= \musicalpitch { -2 2 -2 }
	Essess 	= \musicalpitch { -2 2 -2 }
	Es 	= \musicalpitch { -2 2 -1 }
	Ess 	= \musicalpitch { -2 2 -1 }
	E 	= \musicalpitch { -2 2 0 }
	E 	= \musicalpitch { -2 2 0 }
	Eis 	= \musicalpitch { -2 2 1 }
	Eiss 	= \musicalpitch { -2 2 1 }
	Eisis 	= \musicalpitch { -2 2 2 }
	Eississ = \musicalpitch { -2 2 2 }
	Feses 	= \musicalpitch { -2 3 -2 }
	Fessess = \musicalpitch { -2 3 -2 }
	Fes 	= \musicalpitch { -2 3 -1 }
	Fess 	= \musicalpitch { -2 3 -1 }
	F 	= \musicalpitch { -2 3 0 }
	Fis 	= \musicalpitch { -2 3 1 }
	Fiss 	= \musicalpitch { -2 3 1 }
	Fisis 	= \musicalpitch { -2 3 2 }
	Fississ = \musicalpitch { -2 3 2 }
	Geses 	= \musicalpitch { -2 4 -2 }
	Gessess = \musicalpitch { -2 4 -2 }
	Ges 	= \musicalpitch { -2 4 -1 }
	Gess 	= \musicalpitch { -2 4 -1 }
	G 	= \musicalpitch { -2 4 0 }
	Gis 	= \musicalpitch { -2 4 1 }
	Giss 	= \musicalpitch { -2 4 1 }
	Gisis 	= \musicalpitch { -2 4 2 }
	Gississ = \musicalpitch { -2 4 2 }
	Aeses 	= \musicalpitch { -2 5 -2 }
	Aessess = \musicalpitch { -2 5 -2 }
	Ases 	= \musicalpitch { -2 5 -2 }
	Assess 	= \musicalpitch { -2 5 -2 }
	Aes 	= \musicalpitch { -2 5 -1 }
	Aess 	= \musicalpitch { -2 5 -1 }
	As 	= \musicalpitch { -2 5 -1 }
	Ass 	= \musicalpitch { -2 5 -1 }
	A 	= \musicalpitch { -2 5 0 }
	A 	= \musicalpitch { -2 5 0 }
	Ais 	= \musicalpitch { -2 5 1 }
	Aiss 	= \musicalpitch { -2 5 1 }
	Aisis 	= \musicalpitch { -2 5 2 }
	Aississ = \musicalpitch { -2 5 2 }
	Bes 	= \musicalpitch { -2 6 -2 }
	Bess 	= \musicalpitch { -2 6 -2 }
	B 	= \musicalpitch { -2 6 -1 }
	H 	= \musicalpitch { -2 6 0 }
	His 	= \musicalpitch { -2 6 1 }
	Hiss 	= \musicalpitch { -2 6 1 }
	Hisis 	= \musicalpitch { -2 6 2 }
	Hississ = \musicalpitch { -2 6 2 }

}



