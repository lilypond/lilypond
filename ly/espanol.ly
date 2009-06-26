%{

 Common Spanish names for notes. "b" means flat (bemol), "s" means sharp (sostenido).
 Adapted from italiano.ly.

 English: a   b   c   d   e   f   g
 Spanish: la  si  do  re  mi  fa  sol

%}

%{

 contributed by Carlos García Suárez <<cgscqmp@terra.es>>
 
 and contributed by Maximiliano G. G. <<mxgdvg@yahoo.it>> con:
 doble alteración alternativa, x para doble sostenido
 cuartos de tono: tcb, cb, cs, tcs

%}

pitchnamesEspanol = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dotcb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(docb . ,(ly:make-pitch -1 0 SEMI-FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(docs . ,(ly:make-pitch -1 0 SEMI-SHARP))
	(dos . ,(ly:make-pitch -1 0 SHARP))
	(dotcs . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
	(doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(dox . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(retcb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(recb . ,(ly:make-pitch -1 1 SEMI-FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(recs . ,(ly:make-pitch -1 1 SEMI-SHARP))
	(res . ,(ly:make-pitch -1 1 SHARP))
	(retcs . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
	(ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(rex . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mitcb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(micb . ,(ly:make-pitch -1 2 SEMI-FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(mics . ,(ly:make-pitch -1 2 SEMI-SHARP))
	(mis . ,(ly:make-pitch -1 2 SHARP))
	(mitcs . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
	(miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(mix . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fatcb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(facb . ,(ly:make-pitch -1 3 SEMI-FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(facs . ,(ly:make-pitch -1 3 SEMI-SHARP))
	(fas . ,(ly:make-pitch -1 3 SHARP))
	(fatcs . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
	(fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(fax . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(soltcb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(solcb . ,(ly:make-pitch -1 4 SEMI-FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(solcs . ,(ly:make-pitch -1 4 SEMI-SHARP))
	(sols . ,(ly:make-pitch -1 4 SHARP))
	(soltcs . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
	(solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(solx . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(latcb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(lacb . ,(ly:make-pitch -1 5 SEMI-FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lacs . ,(ly:make-pitch -1 5 SEMI-SHARP))
	(las . ,(ly:make-pitch -1 5 SHARP))
	(latcs . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
	(lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(lax . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sitcb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(sicb . ,(ly:make-pitch -1 6 SEMI-FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sics . ,(ly:make-pitch -1 6 SEMI-SHARP))
	(sis . ,(ly:make-pitch -1 6 SHARP))
	(sitcs . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
	(siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
	(six . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

)

pitchnames = \pitchnamesEspanol

\version "2.12.0"

#(ly:parser-set-note-names parser pitchnames)
