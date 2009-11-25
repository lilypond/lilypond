%%%% common Finnish names for notes
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2001--2009 Heikki Junes <heikki.junes@hut.fi>
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

  es   = flat
  eses = double-flat

  is   = sharp
  isis = double-sharp

  English: c  d  e  f  g  a  bf b
  Finnish: c  d  e  f  g  a  b  h

  Adapted from svenska.ly

%}

pitchnamesSuomi = #`(
	(ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(ces . ,(ly:make-pitch -1 0 FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(cis . ,(ly:make-pitch -1 0 SHARP))
	(cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(des . ,(ly:make-pitch -1 1 FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(dis . ,(ly:make-pitch -1 1 SHARP))
	(disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(es . ,(ly:make-pitch -1 2 FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(eis . ,(ly:make-pitch -1 2 SHARP))
	(eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fes . ,(ly:make-pitch -1 3 FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fis . ,(ly:make-pitch -1 3 SHARP))
	(fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(ges . ,(ly:make-pitch -1 4 FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(gis . ,(ly:make-pitch -1 4 SHARP))
	(gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(asas . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))   ;;non-standard name for asas
	(as . ,(ly:make-pitch -1 5 FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(ais . ,(ly:make-pitch -1 5 SHARP))
	(aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(bb . ,(ly:make-pitch -1 6 DOUBLE-FLAT)) ;; should be bes. Kept for downwards compatibility
	(bes . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(heses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))  ;;non-standard name for bb
	(b . ,(ly:make-pitch -1 6 FLAT))
	(h . ,(ly:make-pitch -1 6 NATURAL))
	(his . ,(ly:make-pitch -1 6 SHARP))
	(hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesSuomi

#(ly:parser-set-note-names parser pitchnames)
