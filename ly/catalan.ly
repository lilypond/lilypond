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
\notenames #'(
	(dobb . ( -1 0 -2 ))
	(dob . ( -1 0 -1 ))
	(do . ( -1 0 0 ))
	(dod . ( -1 0 1 ))
	(dodd . ( -1 0 2 ))
	(rebb . ( -1 1 -2 ))
	(reb . ( -1 1 -1 ))
	(re . ( -1 1 0 ))
	(red . ( -1 1 1 ))
	(redd . ( -1 1 2 ))
	(mibb . ( -1 2 -2 ))
	(mib . ( -1 2 -1 ))
	(mi . ( -1 2 0 ))
	(mid . ( -1 2 1 ))
	(midd . ( -1 2 2 ))
	(fabb . ( -1 3 -2 ))
	(fab . ( -1 3 -1 ))
	(fa . ( -1 3 0 ))
	(fad . ( -1 3 1 ))
	(fadd . ( -1 3 2 ))
	(solbb . ( -1 4 -2 ))
	(solb . ( -1 4 -1 ))
	(sol . ( -1 4 0 ))
	(sold . ( -1 4 1 ))
	(soldd . ( -1 4 2 ))
	(labb . ( -1 5 -2 ))
	(lab . ( -1 5 -1 ))
	(la . ( -1 5 0 ))
	(lad . ( -1 5 1 ))
	(ladd . ( -1 5 2 ))
	(sibb . ( -1 6 -2 ))
	(sib . ( -1 6 -1 ))
	(si . ( -1 6 0 ))
	(sid . ( -1 6 1 ))
	(sidd . ( -1 6 2 ))
	(dos . ( -1 0 1 ))
	(doss . ( -1 0 2 ))
	(res . ( -1 1 1 ))
	(ress . ( -1 1 2 ))
	(mis . ( -1 2 1 ))
	(miss . ( -1 2 2 ))
	(fas . ( -1 3 1 ))
	(fass . ( -1 3 2 ))
	(sols . ( -1 4 1 ))
	(solss . ( -1 4 2 ))
	(las . ( -1 5 1 ))
	(lass . ( -1 5 2 ))
	(sis . ( -1 6 1 ))
	(siss . ( -1 6 2 ))
)



\version "1.3.42";
