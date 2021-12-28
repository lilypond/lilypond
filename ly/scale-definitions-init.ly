%%%% Scales.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


major = #`(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . 0)
  )

minor = #`(
    (0 . 0)
    (1 . 0)
    (2 . ,FLAT)
    (3 . 0)
    (4 . 0)
    (5 . ,FLAT)
    (6 . ,FLAT)
    )


ionian = #`(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . 0)
  )


locrian = #`(
    (0 . 0)
    (1 . ,FLAT)
    (2 . ,FLAT)
    (3 . 0)
    (4 . ,FLAT)
    (5 . ,FLAT)
    (6 . ,FLAT)
  )


aeolian = #`(
    (0 . 0)
    (1 . 0)
    (2 . ,FLAT)
    (3 . 0)
    (4 . 0)
    (5 . ,FLAT)
    (6 . ,FLAT)
    )


mixolydian = #`(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . ,FLAT)
  )


lydian = #`(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . ,SHARP)
    (4 . 0)
    (5 . 0)
    (6 . 0)
  )


phrygian = #`(
    (0 . 0)
    (1 . ,FLAT)
    (2 . ,FLAT)
    (3 . 0)
    (4 . 0)
    (5 . ,FLAT)
    (6 . ,FLAT)
)


dorian = #`(
    (0 . 0)
    (1 . 0)
    (2 . ,FLAT)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . ,FLAT)
  )
