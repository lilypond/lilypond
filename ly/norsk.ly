
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
	(ceses . ,(ly:make-pitch -1 0 -2 ))
	(cessess . ,(ly:make-pitch -1 0 -2 ))
	(ces . ,(ly:make-pitch -1 0 -1 ))
	(cess . ,(ly:make-pitch -1 0 -1 ))
	(c . ,(ly:make-pitch -1 0 0 ))
	(cis . ,(ly:make-pitch -1 0 1 ))
	(ciss . ,(ly:make-pitch -1 0 1 ))
	(cisis . ,(ly:make-pitch -1 0 2 ))
	(cississ . ,(ly:make-pitch -1 0 2 ))
	(deses . ,(ly:make-pitch -1 1 -2 ))
	(dessess . ,(ly:make-pitch -1 1 -2 ))
	(des . ,(ly:make-pitch -1 1 -1 ))
	(dess . ,(ly:make-pitch -1 1 -1 ))
	(d . ,(ly:make-pitch -1 1 0 ))
	(dis . ,(ly:make-pitch -1 1 1 ))
	(diss . ,(ly:make-pitch -1 1 1 ))
	(disis . ,(ly:make-pitch -1 1 2 ))
	(dississ . ,(ly:make-pitch -1 1 2 ))
	(eeses . ,(ly:make-pitch -1 2 -2 ))
	(eessess . ,(ly:make-pitch -1 2 -2 ))
	(eses . ,(ly:make-pitch -1 2 -2 ))
	(essess . ,(ly:make-pitch -1 2 -2 ))
	(ees . ,(ly:make-pitch -1 2 -1 ))
	(eess . ,(ly:make-pitch -1 2 -1 ))
	(es . ,(ly:make-pitch -1 2 -1 ))
	(ess . ,(ly:make-pitch -1 2 -1 ))
	(e . ,(ly:make-pitch -1 2 0 ))
	(eis . ,(ly:make-pitch -1 2 1 ))
	(eiss . ,(ly:make-pitch -1 2 1 ))
	(eisis . ,(ly:make-pitch -1 2 2 ))
	(eississ . ,(ly:make-pitch -1 2 2 ))
	(feses . ,(ly:make-pitch -1 3 -2 ))
	(fessess . ,(ly:make-pitch -1 3 -2 ))
	(fes . ,(ly:make-pitch -1 3 -1 ))
	(fess . ,(ly:make-pitch -1 3 -1 ))
	(f . ,(ly:make-pitch -1 3 0 ))
	(fis . ,(ly:make-pitch -1 3 1 ))
	(fiss . ,(ly:make-pitch -1 3 1 ))
	(fisis . ,(ly:make-pitch -1 3 2 ))
	(fississ . ,(ly:make-pitch -1 3 2 ))
	(geses . ,(ly:make-pitch -1 4 -2 ))
	(gessess . ,(ly:make-pitch -1 4 -2 ))
	(ges . ,(ly:make-pitch -1 4 -1 ))
	(gess . ,(ly:make-pitch -1 4 -1 ))
	(g . ,(ly:make-pitch -1 4 0 ))
	(g . ,(ly:make-pitch -1 4 0 ))
	(gis . ,(ly:make-pitch -1 4 1 ))
	(giss . ,(ly:make-pitch -1 4 1 ))
	(gisis . ,(ly:make-pitch -1 4 2 ))
	(gississ . ,(ly:make-pitch -1 4 2 ))
	(aeses . ,(ly:make-pitch -1 5 -2 ))
	(aessess . ,(ly:make-pitch -1 5 -2 ))
	(ases . ,(ly:make-pitch -1 5 -2 ))
	(assess . ,(ly:make-pitch -1 5 -2 ))
	(aes . ,(ly:make-pitch -1 5 -1 ))
	(aess . ,(ly:make-pitch -1 5 -1 ))
	(as . ,(ly:make-pitch -1 5 -1 ))
	(ass . ,(ly:make-pitch -1 5 -1 ))
	(a . ,(ly:make-pitch -1 5 0 ))
	(ais . ,(ly:make-pitch -1 5 1 ))
	(aiss . ,(ly:make-pitch -1 5 1 ))
	(aisis . ,(ly:make-pitch -1 5 2 ))
	(aississ . ,(ly:make-pitch -1 5 2 ))
	(bes . ,(ly:make-pitch -1 6 -2 ))
	(bess . ,(ly:make-pitch -1 6 -2 ))
	(b . ,(ly:make-pitch -1 6 -1 ))
	(b . ,(ly:make-pitch -1 6 -1 ))
	(h . ,(ly:make-pitch -1 6 0 ))
	(his . ,(ly:make-pitch -1 6 1 ))
	(hiss . ,(ly:make-pitch -1 6 1 ))
	(hisis . ,(ly:make-pitch -1 6 2 ))
	(hississ . ,(ly:make-pitch -1 6 2 ))
)



\version "1.7.3"
