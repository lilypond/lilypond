%{

 Common catalan names for notes. "b" means flat (bemoll), "d" means sharp (diesi).
 Also "s" is included for sharp (sostingut).  Both "diesi" and "sostingut" are approved
 by "Diccionari de la Llengua Catalana" from "Institut d'Estudis Catalans".

 Adapted from italiano.ly.

 English: a   b   c   d   e   f   g
 Catalan: la  si  do  re  mi  fa  sol

 For spanish naming just use the sharp sign "s" (sostenido) instead of "s"
(sostenido) .
%}

% contributed by Jaume Obrador <jobrador@ipc4.uib.es>


% For using "sostingut" notation, which is also correct
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
	(dos . ,(make-pitch -1 0 1 ))
	(doss . ,(make-pitch -1 0 2 ))
	(res . ,(make-pitch -1 1 1 ))
	(ress . ,(make-pitch -1 1 2 ))
	(mis . ,(make-pitch -1 2 1 ))
	(miss . ,(make-pitch -1 2 2 ))
	(fas . ,(make-pitch -1 3 1 ))
	(fass . ,(make-pitch -1 3 2 ))
	(sols . ,(make-pitch -1 4 1 ))
	(solss . ,(make-pitch -1 4 2 ))
	(las . ,(make-pitch -1 5 1 ))
	(lass . ,(make-pitch -1 5 2 ))
	(sis . ,(make-pitch -1 6 1 ))
	(siss . ,(make-pitch -1 6 2 ))
)



\version "1.3.110";
