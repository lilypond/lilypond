% common german names for notes. "es" means flat, "is" means sharp
%
% by Roland Meier <<meier@informatik.th-darmstadt.de>>
% based on swedish.ly by Mats Bengtsson.

% 1999/06/09 Bjoern Jacke <<bjoern.jacke@gmx.de>>
%            added asas and marked ases as `unusual'


pitchnamesDeutsch = #`(
	(ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(ces . ,(ly:make-pitch -1 0 FLAT))
	(ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(cis . ,(ly:make-pitch -1 0 SHARP))
	(cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
	(cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(des . ,(ly:make-pitch -1 1 FLAT))
	(deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(dis . ,(ly:make-pitch -1 1 SHARP))
	(dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
	(disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(es . ,(ly:make-pitch -1 2 FLAT))
	(eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(eis . ,(ly:make-pitch -1 2 SHARP))
	(eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
	(eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fes . ,(ly:make-pitch -1 3 FLAT))
	(feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fis . ,(ly:make-pitch -1 3 SHARP))
	(fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
	(fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(ges . ,(ly:make-pitch -1 4 FLAT))
	(geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(gis . ,(ly:make-pitch -1 4 SHARP))
	(gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
	(gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(asas . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))   ;;non-standard name for asas
	(as . ,(ly:make-pitch -1 5 FLAT))
	(aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(ais . ,(ly:make-pitch -1 5 SHARP))
	(aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
	(aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(heses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(b . ,(ly:make-pitch -1 6 FLAT))
	(beh . ,(ly:make-pitch -1 6 SEMI-FLAT))
	(h . ,(ly:make-pitch -1 6 NATURAL))
	(his . ,(ly:make-pitch -1 6 SHARP))
	(hih . ,(ly:make-pitch -1 6 SEMI-SHARP))
	(hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)


pitchnames = \pitchnamesDeutsch

\version "2.7.39"

#(ly:parser-set-note-names parser pitchnames)
