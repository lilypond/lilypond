%{

 Common Portuguese names for notes. "b" means flat (bemol), "s" means
 sharp (sustenido).  Adapted from espanol.ly and italian.ly.

 English: a   b   c   d   e   f   g
 Portuguese: la  si  do  re  mi  fa  sol

%}

% contributed by Pedro Kröger <<kroeger@pedrokroeger.net>>


pitchnamesPortuguese = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dobtqt . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(dobqt . ,(ly:make-pitch -1 0 SEMI-FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dosqt . ,(ly:make-pitch -1 0 SEMI-SHARP))
	(dos . ,(ly:make-pitch -1 0 SHARP))
	(dostqt . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
	(doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(rebtqt . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(rebqt . ,(ly:make-pitch -1 1 SEMI-FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(resqt . ,(ly:make-pitch -1 1 SEMI-SHARP))
	(res . ,(ly:make-pitch -1 1 SHARP))
	(restqt . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
	(ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mibtqt . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(mibqt . ,(ly:make-pitch -1 2 SEMI-FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(misqt . ,(ly:make-pitch -1 2 SEMI-SHARP))
	(mis . ,(ly:make-pitch -1 2 SHARP))
	(mistqt . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
	(miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fabtqt . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fabqt . ,(ly:make-pitch -1 3 SEMI-FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fasqt . ,(ly:make-pitch -1 3 SEMI-SHARP))
	(fas . ,(ly:make-pitch -1 3 SHARP))
	(fastqt . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
	(fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solbtqt . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(solbqt . ,(ly:make-pitch -1 4 SEMI-FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(solsqt . ,(ly:make-pitch -1 4 SEMI-SHARP))
	(sols . ,(ly:make-pitch -1 4 SHARP))
	(solstqt . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
	(solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(labtqt . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(labqt . ,(ly:make-pitch -1 5 SEMI-FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lasqt . ,(ly:make-pitch -1 5 SEMI-SHARP))
	(las . ,(ly:make-pitch -1 5 SHARP))
	(lastqt . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
	(lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sibtqt . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(sibqt . ,(ly:make-pitch -1 6 SEMI-FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sisqt . ,(ly:make-pitch -1 6 SEMI-SHARP))
	(sis . ,(ly:make-pitch -1 6 SHARP))
	(sistqt . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
	(siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

)

pitchnames = \pitchnamesPortuguese

\version "2.12.0"

#(ly:parser-set-note-names parser pitchnames)
