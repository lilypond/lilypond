
% 	Idx norsk.ly,v 1.1 1998/03/25 11:38:28 arvidg Exp arvidg $	

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
 bugs in here please tell me (or send patches) if you find any.

                                  Arvid Gr=F8tting <arvidg@ifi.uio.no>

%}

\pitchnames #`(
	(ceses . ,(make-pitch -1 0 -2 ))
	(cessess . ,(make-pitch -1 0 -2 ))
	(ces . ,(make-pitch -1 0 -1 ))
	(cess . ,(make-pitch -1 0 -1 ))
	(c . ,(make-pitch -1 0 0 ))
	(cis . ,(make-pitch -1 0 1 ))
	(ciss . ,(make-pitch -1 0 1 ))
	(cisis . ,(make-pitch -1 0 2 ))
	(cississ . ,(make-pitch -1 0 2 ))
	(deses . ,(make-pitch -1 1 -2 ))
	(dessess . ,(make-pitch -1 1 -2 ))
	(des . ,(make-pitch -1 1 -1 ))
	(dess . ,(make-pitch -1 1 -1 ))
	(d . ,(make-pitch -1 1 0 ))
	(dis . ,(make-pitch -1 1 1 ))
	(diss . ,(make-pitch -1 1 1 ))
	(disis . ,(make-pitch -1 1 2 ))
	(dississ . ,(make-pitch -1 1 2 ))
	(eeses . ,(make-pitch -1 2 -2 ))
	(eessess . ,(make-pitch -1 2 -2 ))
	(eses . ,(make-pitch -1 2 -2 ))
	(essess . ,(make-pitch -1 2 -2 ))
	(ees . ,(make-pitch -1 2 -1 ))
	(eess . ,(make-pitch -1 2 -1 ))
	(es . ,(make-pitch -1 2 -1 ))
	(ess . ,(make-pitch -1 2 -1 ))
	(e . ,(make-pitch -1 2 0 ))
	(eis . ,(make-pitch -1 2 1 ))
	(eiss . ,(make-pitch -1 2 1 ))
	(eisis . ,(make-pitch -1 2 2 ))
	(eississ . ,(make-pitch -1 2 2 ))
	(feses . ,(make-pitch -1 3 -2 ))
	(fessess . ,(make-pitch -1 3 -2 ))
	(fes . ,(make-pitch -1 3 -1 ))
	(fess . ,(make-pitch -1 3 -1 ))
	(f . ,(make-pitch -1 3 0 ))
	(fis . ,(make-pitch -1 3 1 ))
	(fiss . ,(make-pitch -1 3 1 ))
	(fisis . ,(make-pitch -1 3 2 ))
	(fississ . ,(make-pitch -1 3 2 ))
	(geses . ,(make-pitch -1 4 -2 ))
	(gessess . ,(make-pitch -1 4 -2 ))
	(ges . ,(make-pitch -1 4 -1 ))
	(gess . ,(make-pitch -1 4 -1 ))
	(g . ,(make-pitch -1 4 0 ))
	(g . ,(make-pitch -1 4 0 ))
	(gis . ,(make-pitch -1 4 1 ))
	(giss . ,(make-pitch -1 4 1 ))
	(gisis . ,(make-pitch -1 4 2 ))
	(gississ . ,(make-pitch -1 4 2 ))
	(aeses . ,(make-pitch -1 5 -2 ))
	(aessess . ,(make-pitch -1 5 -2 ))
	(ases . ,(make-pitch -1 5 -2 ))
	(assess . ,(make-pitch -1 5 -2 ))
	(aes . ,(make-pitch -1 5 -1 ))
	(aess . ,(make-pitch -1 5 -1 ))
	(as . ,(make-pitch -1 5 -1 ))
	(ass . ,(make-pitch -1 5 -1 ))
	(a . ,(make-pitch -1 5 0 ))
	(ais . ,(make-pitch -1 5 1 ))
	(aiss . ,(make-pitch -1 5 1 ))
	(aisis . ,(make-pitch -1 5 2 ))
	(aississ . ,(make-pitch -1 5 2 ))
	(bes . ,(make-pitch -1 6 -2 ))
	(bess . ,(make-pitch -1 6 -2 ))
	(b . ,(make-pitch -1 6 -1 ))
	(b . ,(make-pitch -1 6 -1 ))
	(h . ,(make-pitch -1 6 0 ))
	(his . ,(make-pitch -1 6 1 ))
	(hiss . ,(make-pitch -1 6 1 ))
	(hisis . ,(make-pitch -1 6 2 ))
	(hississ . ,(make-pitch -1 6 2 ))
)



\version "1.5.49"
