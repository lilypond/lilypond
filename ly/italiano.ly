%{
 Common italian names for notes. "b" means flat (bemolle), "d" means sharp (diesis)
 Adapted from dutch.ly.

 English: a   b   c   d   e   f   g
 Italian: la  si  do  re  mi  fa  sol

 For french naming just change 'do' in 'ut'.
%}

% contributed by Paolo Zuliani <zuliap@easynet.it>

\pitchnames #`(
	(dobb . ,(ly:make-pitch -1 0 -2 ))
	(dob . ,(ly:make-pitch -1 0 -1 ))
	(do . ,(ly:make-pitch -1 0 0 ))
	(dod . ,(ly:make-pitch -1 0 1 ))
	(dodd . ,(ly:make-pitch -1 0 2 ))
	(rebb . ,(ly:make-pitch -1 1 -2 ))
	(reb . ,(ly:make-pitch -1 1 -1 ))
	(re . ,(ly:make-pitch -1 1 0 ))
	(red . ,(ly:make-pitch -1 1 1 ))
	(redd . ,(ly:make-pitch -1 1 2 ))
	(mibb . ,(ly:make-pitch -1 2 -2 ))
	(mib . ,(ly:make-pitch -1 2 -1 ))
	(mi . ,(ly:make-pitch -1 2 0 ))
	(mid . ,(ly:make-pitch -1 2 1 ))
	(midd . ,(ly:make-pitch -1 2 2 ))
	(fabb . ,(ly:make-pitch -1 3 -2 ))
	(fab . ,(ly:make-pitch -1 3 -1 ))
	(fa . ,(ly:make-pitch -1 3 0 ))
	(fad . ,(ly:make-pitch -1 3 1 ))
	(fadd . ,(ly:make-pitch -1 3 2 ))
	(solbb . ,(ly:make-pitch -1 4 -2 ))
	(solb . ,(ly:make-pitch -1 4 -1 ))
	(sol . ,(ly:make-pitch -1 4 0 ))
	(sold . ,(ly:make-pitch -1 4 1 ))
	(soldd . ,(ly:make-pitch -1 4 2 ))
	(labb . ,(ly:make-pitch -1 5 -2 ))
	(lab . ,(ly:make-pitch -1 5 -1 ))
	(la . ,(ly:make-pitch -1 5 0 ))
	(lad . ,(ly:make-pitch -1 5 1 ))
	(ladd . ,(ly:make-pitch -1 5 2 ))
	(sibb . ,(ly:make-pitch -1 6 -2 ))
	(sib . ,(ly:make-pitch -1 6 -1 ))
	(si . ,(ly:make-pitch -1 6 0 ))
	(sid . ,(ly:make-pitch -1 6 1 ))
	(sidd . ,(ly:make-pitch -1 6 2 ))
)

\version "1.7.3"
