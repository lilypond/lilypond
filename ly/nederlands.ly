\version "2.12.0"
%{
 common dutch names for notes. es means flat, is means sharp

notenames should only contain letters. No digits or punctuation.

%}

dutchPitchnames = #`(
	(ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))

	(ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
	(ces . ,(ly:make-pitch -1 0 FLAT))
	(ceseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(cis . ,(ly:make-pitch -1 0 SHARP))
	(cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
	(cisih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
	(cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))

	(deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
	(des . ,(ly:make-pitch -1 1 FLAT))
	(deseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(dis . ,(ly:make-pitch -1 1 SHARP))
	(dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
	(disih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
	(disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))

	(eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
	(ees . ,(ly:make-pitch -1 2 FLAT))
	(eeseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
	(es . ,(ly:make-pitch -1 2 FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(eis . ,(ly:make-pitch -1 2 SHARP))
	(eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
	(eisih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
	(eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))

	(feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
	(fes . ,(ly:make-pitch -1 3 FLAT))
	(feseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fis . ,(ly:make-pitch -1 3 SHARP))
	(fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
	(fisih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
	(fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))

	(geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
	(ges . ,(ly:make-pitch -1 4 FLAT))
	(geseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(gis . ,(ly:make-pitch -1 4 SHARP))
	(gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
	(gisih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
	(gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))

	(aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
	(aes . ,(ly:make-pitch -1 5 FLAT))
	(aeseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
	(as . ,(ly:make-pitch -1 5 FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(ais . ,(ly:make-pitch -1 5 SHARP))
	(aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
	(aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
	(aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(beses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))

	(beh . ,(ly:make-pitch -1 6 SEMI-FLAT))
	(bes . ,(ly:make-pitch -1 6 FLAT))
	(beseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
	(b . ,(ly:make-pitch -1 6 NATURAL))
	(bis . ,(ly:make-pitch -1 6 SHARP))
	(bih . ,(ly:make-pitch -1 6 SEMI-SHARP))
	(bisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
	(bisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
	)


pitchnames = \dutchPitchnames 
#(ly:parser-set-note-names parser pitchnames)
