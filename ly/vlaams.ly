%%%% common Flemish names for notes
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2004--2009 Hendrik Maryns <hendrik.maryns@ugent.be>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.12.0"

%{

  b  = flat (bemol)
  bb = double-flat

  k  = sharp (kruis)
  kk = double-sharp

  English: c   d   e   f   g   a   b
  Flemish: do  re  mi  fa  sol la  si

  Adapted from espanol.ly.

%}

pitchnamesVlaams = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dok . ,(ly:make-pitch -1 0 SHARP))
	(dokk . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(rek . ,(ly:make-pitch -1 1 SHARP))
	(rekk . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(mik . ,(ly:make-pitch -1 2 SHARP))
	(mikk . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fak . ,(ly:make-pitch -1 3 SHARP))
	(fakk . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(solk . ,(ly:make-pitch -1 4 SHARP))
	(solkk . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lak . ,(ly:make-pitch -1 5 SHARP))
	(lakk . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sik . ,(ly:make-pitch -1 6 SHARP))
	(sikk . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesVlaams

#(ly:parser-set-note-names parser pitchnames)
