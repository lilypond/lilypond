
% 	$Id: norsk.ly,v 1.5 2000/06/16 14:10:51 fred Exp $	

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

\notenames #'(
	(ceses . ( -1 0 -2 ))
	(cessess . ( -1 0 -2 ))
	(ces . ( -1 0 -1 ))
	(cess . ( -1 0 -1 ))
	(c . ( -1 0 0 ))
	(cis . ( -1 0 1 ))
	(ciss . ( -1 0 1 ))
	(cisis . ( -1 0 2 ))
	(cississ . ( -1 0 2 ))
	(deses . ( -1 1 -2 ))
	(dessess . ( -1 1 -2 ))
	(des . ( -1 1 -1 ))
	(dess . ( -1 1 -1 ))
	(d . ( -1 1 0 ))
	(dis . ( -1 1 1 ))
	(diss . ( -1 1 1 ))
	(disis . ( -1 1 2 ))
	(dississ . ( -1 1 2 ))
	(eeses . ( -1 2 -2 ))
	(eessess . ( -1 2 -2 ))
	(eses . ( -1 2 -2 ))
	(essess . ( -1 2 -2 ))
	(ees . ( -1 2 -1 ))
	(eess . ( -1 2 -1 ))
	(es . ( -1 2 -1 ))
	(ess . ( -1 2 -1 ))
	(e . ( -1 2 0 ))
	(eis . ( -1 2 1 ))
	(eiss . ( -1 2 1 ))
	(eisis . ( -1 2 2 ))
	(eississ . ( -1 2 2 ))
	(feses . ( -1 3 -2 ))
	(fessess . ( -1 3 -2 ))
	(fes . ( -1 3 -1 ))
	(fess . ( -1 3 -1 ))
	(f . ( -1 3 0 ))
	(fis . ( -1 3 1 ))
	(fiss . ( -1 3 1 ))
	(fisis . ( -1 3 2 ))
	(fississ . ( -1 3 2 ))
	(geses . ( -1 4 -2 ))
	(gessess . ( -1 4 -2 ))
	(ges . ( -1 4 -1 ))
	(gess . ( -1 4 -1 ))
	(g . ( -1 4 0 ))
	(g . ( -1 4 0 ))
	(gis . ( -1 4 1 ))
	(giss . ( -1 4 1 ))
	(gisis . ( -1 4 2 ))
	(gississ . ( -1 4 2 ))
	(aeses . ( -1 5 -2 ))
	(aessess . ( -1 5 -2 ))
	(ases . ( -1 5 -2 ))
	(assess . ( -1 5 -2 ))
	(aes . ( -1 5 -1 ))
	(aess . ( -1 5 -1 ))
	(as . ( -1 5 -1 ))
	(ass . ( -1 5 -1 ))
	(a . ( -1 5 0 ))
	(ais . ( -1 5 1 ))
	(aiss . ( -1 5 1 ))
	(aisis . ( -1 5 2 ))
	(aississ . ( -1 5 2 ))
	(bes . ( -1 6 -2 ))
	(bess . ( -1 6 -2 ))
	(b . ( -1 6 -1 ))
	(b . ( -1 6 -1 ))
	(h . ( -1 6 0 ))
	(his . ( -1 6 1 ))
	(hiss . ( -1 6 1 ))
	(hisis . ( -1 6 2 ))
	(hississ . ( -1 6 2 ))
)



\version "1.3.59";
