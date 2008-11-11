\version "2.10.0" %hier stond 2.1.36, mocht ik dat wel veranderen?

%{

 Common Flemish names for notes. "b" means flat (bemol), "k" means sharp (kruis).
 Adapted from espanol.ly.

 English: a   b   c   d   e   f   g
 Flemish: la  si  do  re  mi  fa  sol

%}

% contributed by Hendrik Maryns <<hendrik.maryns@ugent.be>> 06/06/2004


pitchnamesVlaams = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dok . ,(ly:make-pitch -1 0 SHARP))
	(dokk . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(rek . ,(ly:make-pitch -1 1 SHARP))
	(rekk . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(mik . ,(ly:make-pitch -1 2 SHARP))
	(mikk . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fak . ,(ly:make-pitch -1 3 SHARP))
	(fakk . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(solk . ,(ly:make-pitch -1 4 SHARP))
	(solkk . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lak . ,(ly:make-pitch -1 5 SHARP))
	(lakk . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sik . ,(ly:make-pitch -1 6 SHARP))
	(sikk . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesVlaams

#(ly:parser-set-note-names parser pitchnames)
