%%%% italiano.ly -- common Italian names for notes
%%%%
%%%% source file of the GNU LilyPond music typesetter
%%%%
%%%% (c) 1998--2009 Paolo Zuliani <zuliap@easynet.it>
%%%%                Eric Wurbel <wurbel@univ-tln.fr>
%%%%                  (additions for semi-sharps and semi-flats)

\version "2.12.0"

%{

  b   = flat (bemolle)
  bb  = double-flat
  sb  = quarter-tone flat (semi-bemolle)
  bsb = three-quarter-tones flat

  d   = sharp (diesis)
  dd  = double-sharp
  sd  = quarter-tone sharp (semi-diesis)
  dsd = three-quarter-tones sharp

  English: c   d   e   f   g   a   b
  Italian: do  re  mi  fa  sol la  si

  Adapted from nederlands.ly.

  For french naming just change 'do' to 'ut'.

%}

pitchnamesItaliano = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dobsb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(dosb . ,(ly:make-pitch -1 0 SEMI-FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dosd . ,(ly:make-pitch -1 0 SEMI-SHARP))
	(dod . ,(ly:make-pitch -1 0 SHARP))
	(dodsd . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
	(dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(rebsb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(resb . ,(ly:make-pitch -1 1 SEMI-FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(resd . ,(ly:make-pitch -1 1 SEMI-SHARP))
	(red . ,(ly:make-pitch -1 1 SHARP))
	(redsd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
	(redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mibsb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(misb . ,(ly:make-pitch -1 2 SEMI-FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(misd . ,(ly:make-pitch -1 2 SEMI-SHARP))
	(mid . ,(ly:make-pitch -1 2 SHARP))
	(midsd . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
	(midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fabsb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fasb . ,(ly:make-pitch -1 3 SEMI-FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fasd . ,(ly:make-pitch -1 3 SEMI-SHARP))
	(fad . ,(ly:make-pitch -1 3 SHARP))
	(fadsd . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
	(fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solbsb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(solsb . ,(ly:make-pitch -1 4 SEMI-FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(solsd . ,(ly:make-pitch -1 4 SEMI-SHARP))
	(sold . ,(ly:make-pitch -1 4 SHARP))
	(soldsd . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
	(soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(labsb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(lasb . ,(ly:make-pitch -1 5 SEMI-FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lasd . ,(ly:make-pitch -1 5 SEMI-SHARP))
	(lad . ,(ly:make-pitch -1 5 SHARP))
	(ladsd . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
	(ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sibsb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(sisb . ,(ly:make-pitch -1 6 SEMI-FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sisd . ,(ly:make-pitch -1 6 SEMI-SHARP))
	(sid . ,(ly:make-pitch -1 6 SHARP))
	(sidsd . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
	(sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

)

pitchnames = \pitchnamesItaliano

#(ly:parser-set-note-names parser pitchnames)
