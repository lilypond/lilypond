%%%% Template toolkit (voice functions).
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2015--2022 Trevor Daniels <t.daniels@treda.co.uk>
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

%\version "2.19.22"

\include "base-tkit.ly"

make-voice =
#(define-music-function (name) (voice-prefix?)
   (define music (make-id name "Music"))
   (if music
       #{
         \new Voice = #(string-append name "Voice") <<
           #(if KeepAlive KeepAlive)
           #(if Time Time )
           #music
         >>
       #} ))

