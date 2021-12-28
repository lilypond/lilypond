%%%% Predefined dynamic scripts.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2001--2022 Jan Nieuwenhuizen <janneke@gnu.org>,
%%%%                          Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.21.0"

%
% declare the standard dynamic identifiers.
%

#(define (make-dynamic-script str)
   (make-music 'AbsoluteDynamicEvent
              'text str))

%% don't exceed ppppp or fffff; see midi.scm.
ppppp = #(make-dynamic-script "ppppp")
pppp = #(make-dynamic-script "pppp")
ppp = #(make-dynamic-script "ppp")
pp = #(make-dynamic-script "pp")
p = #(make-dynamic-script "p")
mp = #(make-dynamic-script "mp")
mf = #(make-dynamic-script "mf")

%% f is pitch.
"f" = #(make-dynamic-script "f")
ff = #(make-dynamic-script "ff")
fff = #(make-dynamic-script "fff")
ffff = #(make-dynamic-script "ffff")
fffff = #(make-dynamic-script "fffff")
fp = #(make-dynamic-script "fp")
sf = #(make-dynamic-script "sf")
sfp = #(make-dynamic-script "sfp")
sff = #(make-dynamic-script "sff")
sfz = #(make-dynamic-script "sfz")
fz = #(make-dynamic-script "fz")
sp = #(make-dynamic-script "sp")
spp = #(make-dynamic-script "spp")
rfz = #(make-dynamic-script "rfz")
n = #(make-dynamic-script "n")
