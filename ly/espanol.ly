%{

 Common Spanish names for notes. "b" means flat (bemol), "s" means sharp (sostenido).
 Adapted from italiano.ly.

 English: a   b   c   d   e   f   g
 Spanish: la  si  do  re  mi  fa  sol

%}

% contributed by Carlos García Suárez <cgscqmp@terra.es>


% For using "sostingut" notation, which is also correct
\pitchnames #`(
	(dobb . ,(make-pitch -1 0 -2 ))
	(dob . ,(make-pitch -1 0 -1 ))
	(do . ,(make-pitch -1 0 0 ))
	(dos . ,(make-pitch -1 0 1 ))
	(doss . ,(make-pitch -1 0 2 ))

	(rebb . ,(make-pitch -1 1 -2 ))
	(reb . ,(make-pitch -1 1 -1 ))
	(re . ,(make-pitch -1 1 0 ))
	(res . ,(make-pitch -1 1 1 ))
	(ress . ,(make-pitch -1 1 2 ))

	(mibb . ,(make-pitch -1 2 -2 ))
	(mib . ,(make-pitch -1 2 -1 ))
	(mi . ,(make-pitch -1 2 0 ))
	(mis . ,(make-pitch -1 2 1 ))
	(miss . ,(make-pitch -1 2 2 ))

	(fabb . ,(make-pitch -1 3 -2 ))
	(fab . ,(make-pitch -1 3 -1 ))
	(fa . ,(make-pitch -1 3 0 ))
	(fas . ,(make-pitch -1 3 1 ))
	(fass . ,(make-pitch -1 3 2 ))

	(solbb . ,(make-pitch -1 4 -2 ))
	(solb . ,(make-pitch -1 4 -1 ))
	(sol . ,(make-pitch -1 4 0 ))
	(sols . ,(make-pitch -1 4 1 ))
	(solss . ,(make-pitch -1 4 2 ))

	(labb . ,(make-pitch -1 5 -2 ))
	(lab . ,(make-pitch -1 5 -1 ))
	(la . ,(make-pitch -1 5 0 ))
	(las . ,(make-pitch -1 5 1 ))
	(lass . ,(make-pitch -1 5 2 ))

	(sibb . ,(make-pitch -1 6 -2 ))
	(sib . ,(make-pitch -1 6 -1 ))
	(si . ,(make-pitch -1 6 0 ))
	(sis . ,(make-pitch -1 6 1 ))
	(siss . ,(make-pitch -1 6 2 ))
)



\version "1.5.68"
