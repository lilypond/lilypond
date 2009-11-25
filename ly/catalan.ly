%%%% common Catalan names for notes
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1998--2009 Jaume Obrador <jobrador@ipc4.uib.es>
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

  b  = flat (bemoll)
  bb = double-flat

  d  = sharp (diesi)
  dd = double-sharp

  s  = sharp (sostingut)
  ss = double-sharp

  English: c   d   e   f   g   a   b
  Catalan: do  re  mi  fa  sol la  si

  Adapted from italiano.ly.

%}

pitchnamesCatalan = #`(
	(dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(dob . ,(ly:make-pitch -1 0 FLAT))
	(do . ,(ly:make-pitch -1 0 NATURAL))
	(dod . ,(ly:make-pitch -1 0 SHARP))
	(dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(reb . ,(ly:make-pitch -1 1 FLAT))
	(re . ,(ly:make-pitch -1 1 NATURAL))
	(red . ,(ly:make-pitch -1 1 SHARP))
	(redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(mib . ,(ly:make-pitch -1 2 FLAT))
	(mi . ,(ly:make-pitch -1 2 NATURAL))
	(mid . ,(ly:make-pitch -1 2 SHARP))
	(midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fab . ,(ly:make-pitch -1 3 FLAT))
	(fa . ,(ly:make-pitch -1 3 NATURAL))
	(fad . ,(ly:make-pitch -1 3 SHARP))
	(fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(solb . ,(ly:make-pitch -1 4 FLAT))
	(sol . ,(ly:make-pitch -1 4 NATURAL))
	(sold . ,(ly:make-pitch -1 4 SHARP))
	(soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(lab . ,(ly:make-pitch -1 5 FLAT))
	(la . ,(ly:make-pitch -1 5 NATURAL))
	(lad . ,(ly:make-pitch -1 5 SHARP))
	(ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(sib . ,(ly:make-pitch -1 6 FLAT))
	(si . ,(ly:make-pitch -1 6 NATURAL))
	(sid . ,(ly:make-pitch -1 6 SHARP))
	(sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

	;; Now that we have espanol.ly, should these be junked? --jcn
	(dos . ,(ly:make-pitch -1 0 SHARP))
	(doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(res . ,(ly:make-pitch -1 1 SHARP))
	(ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(mis . ,(ly:make-pitch -1 2 SHARP))
	(miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(fas . ,(ly:make-pitch -1 3 SHARP))
	(fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(sols . ,(ly:make-pitch -1 4 SHARP))
	(solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(las . ,(ly:make-pitch -1 5 SHARP))
	(lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(sis . ,(ly:make-pitch -1 6 SHARP))
	(siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesCatalan

#(ly:parser-set-note-names parser pitchnames)
