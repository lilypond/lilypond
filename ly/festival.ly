%%%% Festival singing mode output
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2006--2022 Brailcom, o.p.s.
%%%%
%%%% Author: Milan Zamazal <pdm@brailcom.org>
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

\version "2.23.2"

#(use-modules (lily song))
#(use-modules (srfi srfi-39))

% \festival "filename" { \tempo N = X } { music }
festival =
#(define-music-function (filename tempo music)
   (string? ly:music? ly:music?)
   (output-file music tempo filename)
   music)

% \festivalsyl "filename" { \tempo N = X } { music }
festivalsyl =
#(define-music-function (filename tempo music)
   (string? ly:music? ly:music?)
   (parameterize ((*syllabify* #t))
     (output-file music tempo filename))
   music)
