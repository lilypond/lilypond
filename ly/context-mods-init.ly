%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                    Jan Nieuwenhuizen <janneke@gnu.org>
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

\version "2.15.6"

RemoveEmptyStaves = \with {
  \remove "Axis_group_engraver"
  % If RemoveEmptyStaves is called twice, two
  % Hara_kiri_engravers would be added, which leads to a
  % warning.
  % This code makes sure that no previous Hara_kiri_engraver
  % is left before adding a new one.
  \remove "Hara_kiri_engraver"
  \consists "Hara_kiri_engraver"
  \override VerticalAxisGroup #'remove-empty = ##t
}
