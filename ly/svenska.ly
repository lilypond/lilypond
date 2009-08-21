%%%% svenska.ly -- common Swedish names for notes
%%%%
%%%% source file of the GNU LilyPond music typesetter
%%%%
%%%% (c) 1997--2009 Mats Bengtsson <mabe@violin.s3.kth.se>

\version "2.12.0"

%{

  ess    = flat
  essess = double-flat

  iss    = sharp
  ississ = double-sharp

  English: c  d  e  f  g  a  bf b
  Swedish: c  d  e  f  g  a  b  h

%}

pitchnamesSvenska = #`(
	(cessess . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(cess . ,(ly:make-pitch -1 0 FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(ciss . ,(ly:make-pitch -1 0 SHARP))
	(cississ . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(dessess . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(dess . ,(ly:make-pitch -1 1 FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(diss . ,(ly:make-pitch -1 1 SHARP))
	(dississ . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(essess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(ess . ,(ly:make-pitch -1 2 FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(eiss . ,(ly:make-pitch -1 2 SHARP))
	(eississ . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(fessess . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fess . ,(ly:make-pitch -1 3 FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fiss . ,(ly:make-pitch -1 3 SHARP))
	(fississ . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(gessess . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(gess . ,(ly:make-pitch -1 4 FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(giss . ,(ly:make-pitch -1 4 SHARP))
	(gississ . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(assess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(ass . ,(ly:make-pitch -1 5 FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(aiss . ,(ly:make-pitch -1 5 SHARP))
	(aississ . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(hessess . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(b . ,(ly:make-pitch -1 6 FLAT))
	(h . ,(ly:make-pitch -1 6 NATURAL))
	(hiss . ,(ly:make-pitch -1 6 SHARP))
	(hississ . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesSvenska

#(ly:parser-set-note-names parser pitchnames)
