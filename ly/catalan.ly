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

	;; Now that we have espanol.ly, should these be junked? --jcn
	(dos . ,(ly:make-pitch -1 0 1 ))
	(doss . ,(ly:make-pitch -1 0 2 ))
	(res . ,(ly:make-pitch -1 1 1 ))
	(ress . ,(ly:make-pitch -1 1 2 ))
	(mis . ,(ly:make-pitch -1 2 1 ))
	(miss . ,(ly:make-pitch -1 2 2 ))
	(fas . ,(ly:make-pitch -1 3 1 ))
	(fass . ,(ly:make-pitch -1 3 2 ))
	(sols . ,(ly:make-pitch -1 4 1 ))
	(solss . ,(ly:make-pitch -1 4 2 ))
	(las . ,(ly:make-pitch -1 5 1 ))
	(lass . ,(ly:make-pitch -1 5 2 ))
	(sis . ,(ly:make-pitch -1 6 1 ))
	(siss . ,(ly:make-pitch -1 6 2 ))
)



\version "1.7.3"
