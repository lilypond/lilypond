%{
 Common italian names for notes. "b" means flat (bemolle), "d" means sharp (diesis)
 Adapted from dutch.ly.

 English: a   b   c   d   e   f   g
 Italian: la  si  do  re  mi  fa  sol

 For french naming just change 'do' in 'ut'.
%}

% contributed by Paolo Zuliani <zuliap@easynet.it>

\pitchnames #`(
	(dobb . ,(make-pitch -1 0 -2 ))
	(dob . ,(make-pitch -1 0 -1 ))
	(do . ,(make-pitch -1 0 0 ))
	(dod . ,(make-pitch -1 0 1 ))
	(dodd . ,(make-pitch -1 0 2 ))
	(rebb . ,(make-pitch -1 1 -2 ))
	(reb . ,(make-pitch -1 1 -1 ))
	(re . ,(make-pitch -1 1 0 ))
	(red . ,(make-pitch -1 1 1 ))
	(redd . ,(make-pitch -1 1 2 ))
	(mibb . ,(make-pitch -1 2 -2 ))
	(mib . ,(make-pitch -1 2 -1 ))
	(mi . ,(make-pitch -1 2 0 ))
	(mid . ,(make-pitch -1 2 1 ))
	(midd . ,(make-pitch -1 2 2 ))
	(fabb . ,(make-pitch -1 3 -2 ))
	(fab . ,(make-pitch -1 3 -1 ))
	(fa . ,(make-pitch -1 3 0 ))
	(fad . ,(make-pitch -1 3 1 ))
	(fadd . ,(make-pitch -1 3 2 ))
	(solbb . ,(make-pitch -1 4 -2 ))
	(solb . ,(make-pitch -1 4 -1 ))
	(sol . ,(make-pitch -1 4 0 ))
	(sold . ,(make-pitch -1 4 1 ))
	(soldd . ,(make-pitch -1 4 2 ))
	(labb . ,(make-pitch -1 5 -2 ))
	(lab . ,(make-pitch -1 5 -1 ))
	(la . ,(make-pitch -1 5 0 ))
	(lad . ,(make-pitch -1 5 1 ))
	(ladd . ,(make-pitch -1 5 2 ))
	(sibb . ,(make-pitch -1 6 -2 ))
	(sib . ,(make-pitch -1 6 -1 ))
	(si . ,(make-pitch -1 6 0 ))
	(sid . ,(make-pitch -1 6 1 ))
	(sidd . ,(make-pitch -1 6 2 ))
)

\version "1.3.110";
