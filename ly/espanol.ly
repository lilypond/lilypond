%{

 Common Spanish names for notes. "b" means flat (bemol), "s" means sharp (sostenido).
 Adapted from italiano.ly.

 English: a   b   c   d   e   f   g
 Spanish: la  si  do  re  mi  fa  sol

%}

% contributed by Carlos García Suárez <<cgscqmp@terra.es>>


% For using "sostingut" notation, which is also correct
pitchnamesEspanol = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dos . ,(ly:make-pitch -1 0 SHARP))
	(doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(res . ,(ly:make-pitch -1 1 SHARP))
	(ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(mis . ,(ly:make-pitch -1 2 SHARP))
	(miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fas . ,(ly:make-pitch -1 3 SHARP))
	(fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(sols . ,(ly:make-pitch -1 4 SHARP))
	(solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(las . ,(ly:make-pitch -1 5 SHARP))
	(lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sis . ,(ly:make-pitch -1 6 SHARP))
	(siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesEspanol

\version "2.12.0"

#(ly:parser-set-note-names parser pitchnames)
