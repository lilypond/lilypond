%{
 Common italian names for notes. "b" means flat (bemolle), "d" means sharp (diesis)
 Adapted from dutch.ly.

 English: a   b   c   d   e   f   g
 Italian: la  si  do  re  mi  fa  sol

 For french naming just change 'do' in 'ut'.
%}

% contributed by Paolo Zuliani <<zuliap@easynet.it>>

\pitchnames #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dod . ,(ly:make-pitch -1 0 SHARP))
	(dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(red . ,(ly:make-pitch -1 1 SHARP))
	(redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(mid . ,(ly:make-pitch -1 2 SHARP))
	(midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fad . ,(ly:make-pitch -1 3 SHARP))
	(fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(sold . ,(ly:make-pitch -1 4 SHARP))
	(soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lad . ,(ly:make-pitch -1 5 SHARP))
	(ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sid . ,(ly:make-pitch -1 6 SHARP))
	(sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

\version "1.9.8"
