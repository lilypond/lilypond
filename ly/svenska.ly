%%%% common Swedish names for notes
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1997--2010 Mats Bengtsson <mabe@violin.s3.kth.se>
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
