%{

 Common Spanish names for notes. "b" means flat (bemol), "s" means sharp (sostenido).
 Adapted from italiano.ly.

 English: a   b   c   d   e   f   g
 Spanish: la  si  do  re  mi  fa  sol

%}

% contributed by Carlos García Suárez <cgscqmp@terra.es>


% For using "sostingut" notation, which is also correct
\pitchnames #`(
	(dobb . ,(ly:make-pitch -1 0 -2 ))
	(dob . ,(ly:make-pitch -1 0 -1 ))
	(do . ,(ly:make-pitch -1 0 0 ))
	(dos . ,(ly:make-pitch -1 0 1 ))
	(doss . ,(ly:make-pitch -1 0 2 ))

	(rebb . ,(ly:make-pitch -1 1 -2 ))
	(reb . ,(ly:make-pitch -1 1 -1 ))
	(re . ,(ly:make-pitch -1 1 0 ))
	(res . ,(ly:make-pitch -1 1 1 ))
	(ress . ,(ly:make-pitch -1 1 2 ))

	(mibb . ,(ly:make-pitch -1 2 -2 ))
	(mib . ,(ly:make-pitch -1 2 -1 ))
	(mi . ,(ly:make-pitch -1 2 0 ))
	(mis . ,(ly:make-pitch -1 2 1 ))
	(miss . ,(ly:make-pitch -1 2 2 ))

	(fabb . ,(ly:make-pitch -1 3 -2 ))
	(fab . ,(ly:make-pitch -1 3 -1 ))
	(fa . ,(ly:make-pitch -1 3 0 ))
	(fas . ,(ly:make-pitch -1 3 1 ))
	(fass . ,(ly:make-pitch -1 3 2 ))

	(solbb . ,(ly:make-pitch -1 4 -2 ))
	(solb . ,(ly:make-pitch -1 4 -1 ))
	(sol . ,(ly:make-pitch -1 4 0 ))
	(sols . ,(ly:make-pitch -1 4 1 ))
	(solss . ,(ly:make-pitch -1 4 2 ))

	(labb . ,(ly:make-pitch -1 5 -2 ))
	(lab . ,(ly:make-pitch -1 5 -1 ))
	(la . ,(ly:make-pitch -1 5 0 ))
	(las . ,(ly:make-pitch -1 5 1 ))
	(lass . ,(ly:make-pitch -1 5 2 ))

	(sibb . ,(ly:make-pitch -1 6 -2 ))
	(sib . ,(ly:make-pitch -1 6 -1 ))
	(si . ,(ly:make-pitch -1 6 0 ))
	(sis . ,(ly:make-pitch -1 6 1 ))
	(siss . ,(ly:make-pitch -1 6 2 ))
)



\version "1.7.3"
