%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                          Jan Nieuwenhuizen <janneke@gnu.org>
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

\version "2.16.0"

absolute-volume-alist = #'(
         ("sf" . 1.00)
         ("fffff" . 0.95)
         ("ffff" . 0.92)
         ("fff" . 0.85)
         ("ff" . 0.80)
         ("f" . 0.75)
         ("mf" . 0.68)
         ("mp" . 0.61)
         ("p" . 0.55)
         ("pp" . 0.49)
         ("ppp" . 0.42)
         ("pppp" . 0.34)
         ("ppppp" . 0.25)
       )

#(define (default-dynamic-absolute-volume s)
  (assoc-get s absolute-volume-alist))

instrument-equalizer-alist  = #'(
         ("flute" . (0 . 0.7))
         ("oboe" . (0 . 0.7))
         ("clarinet" . (0 . 0.7))
         ("bassoon" . (0 . 0.6))
         ("french horn" . (0.1 . 0.7))
         ("trumpet" . (0.1 . 0.8))
         ("timpani" . (0.2 . 0.9))
         ("violin" . (0.2 . 1.0))
         ("viola" . (0.1 . 0.7))
         ("cello" . (0.2 . 0.8))
         ("contrabass" . (0.2 . 0.8))
         )

#(define (default-instrument-equalizer s)
  (assoc-get s instrument-equalizer-alist))

\midi {
	\include "performer-init.ly"
}
