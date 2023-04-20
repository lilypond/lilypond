% neume-demo-layout.ly
%%%% A layout for presenting Vaticana style neumes.
%%%%
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2003--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>,
%%%%                          Jürgen Reuter <reuter_j@web.de>
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

\version "2.25.5"

%
% `neumeDemoLayout` defines a layout block suitable for notating pure
% Vaticana style neumes without any other notation symbols such as
% staff lines or clefs.  This layout is useful for engraving neumes
% tables, such as that one in the LilyPond manual section on
% Gregorian ligatures, or for educational works.
%
neumeDemoLayout = \layout {
  \context {
    \Score
    \remove Bar_number_engraver
  }
  \context {
    \Staff
    \remove Clef_engraver
    \remove Key_engraver
    \hide StaffSymbol
    \remove Time_signature_engraver
    \remove Bar_engraver
    \override VerticalAxisGroup.staff-staff-spacing = #'()
  }
  \context {
    \Voice
    \remove Ligature_bracket_engraver
    \consists Vaticana_ligature_engraver
    \override NoteHead.style = #'vaticana.punctum
    \remove Stem_engraver
  }
}
