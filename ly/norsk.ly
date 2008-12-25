
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

                                  Arvid Gr=F8tting <<arvidg@ifi.uio.no>>

%}

pitchnamesNorsk = #`(
	(ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(cessess . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(ces . ,(ly:make-pitch -1 0 FLAT))
	(cess . ,(ly:make-pitch -1 0 FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(cis . ,(ly:make-pitch -1 0 SHARP))
	(ciss . ,(ly:make-pitch -1 0 SHARP))
	(cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(cississ . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(dessess . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(des . ,(ly:make-pitch -1 1 FLAT))
	(dess . ,(ly:make-pitch -1 1 FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(dis . ,(ly:make-pitch -1 1 SHARP))
	(diss . ,(ly:make-pitch -1 1 SHARP))
	(disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(dississ . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(eessess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(essess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(ees . ,(ly:make-pitch -1 2 FLAT))
	(eess . ,(ly:make-pitch -1 2 FLAT))
	(es . ,(ly:make-pitch -1 2 FLAT))
	(ess . ,(ly:make-pitch -1 2 FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(eis . ,(ly:make-pitch -1 2 SHARP))
	(eiss . ,(ly:make-pitch -1 2 SHARP))
	(eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(eississ . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fessess . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fes . ,(ly:make-pitch -1 3 FLAT))
	(fess . ,(ly:make-pitch -1 3 FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fis . ,(ly:make-pitch -1 3 SHARP))
	(fiss . ,(ly:make-pitch -1 3 SHARP))
	(fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(fississ . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(gessess . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(ges . ,(ly:make-pitch -1 4 FLAT))
	(gess . ,(ly:make-pitch -1 4 FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(gis . ,(ly:make-pitch -1 4 SHARP))
	(giss . ,(ly:make-pitch -1 4 SHARP))
	(gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(gississ . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(aessess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(assess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(aes . ,(ly:make-pitch -1 5 FLAT))
	(aess . ,(ly:make-pitch -1 5 FLAT))
	(as . ,(ly:make-pitch -1 5 FLAT))
	(ass . ,(ly:make-pitch -1 5 FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(ais . ,(ly:make-pitch -1 5 SHARP))
	(aiss . ,(ly:make-pitch -1 5 SHARP))
	(aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(aississ . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(bes . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(bess . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(b . ,(ly:make-pitch -1 6 FLAT))
	(b . ,(ly:make-pitch -1 6 FLAT))
	(h . ,(ly:make-pitch -1 6 NATURAL))
	(his . ,(ly:make-pitch -1 6 SHARP))
	(hiss . ,(ly:make-pitch -1 6 SHARP))
	(hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
	(hississ . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)


pitchnames =\pitchnamesNorsk

\version "2.12.0"

#(ly:parser-set-note-names parser pitchnames)
