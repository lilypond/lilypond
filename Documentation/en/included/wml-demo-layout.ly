% wml-demo-layout.ly
%%%% A layout for presenting white mensural ligatures.
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

\version "2.27.0"

% `wmlDemoLayout` defines a layout block suitable for notating pure
% white mensural ligatures without any other notation symbols such as
% staff lines or clefs.  This layout is useful for engraving ligature
% tables, such as those in the LilyPond manual section on
% white mensural ligatures, or for educational works.

#(set-global-staff-size 16)

\layout {
  \context {
    \Score
    \remove Bar_number_engraver
  }
  \context {
    \Staff
    \remove Clef_engraver
    \remove Key_engraver
    \remove Staff_symbol_engraver
    \remove Time_signature_engraver
    \remove Bar_engraver
  }
  \context {
    \Voice
    \remove Ligature_bracket_engraver
    \consists Mensural_ligature_engraver
  }
}

% The macro below helps circumvent various issues.
%
% * At the time of this writing (July 2026), ligatures retain spacing if
%   timing is disabled: contrary to normal notes, there is a large
%   horizontal space (depending on the kind of the ligature) to the right
%   that doesn't vanish (as it should; this is a bug).  For this reason, we
%   use `\with-true-dimensions` to remove it.
%
% * Another reason to use `\with-true-dimensions` is to remove excess
%   vertical whitespace, which would otherwise unnecessarily distort the
%   line spacing within a paragraph.
%
% * For some unknown reason, LilyPond always adds some horizontal space to
%   the left if a cropped image gets produced (probably a bug).  Using
%   `\center-align` prevents that, and `\pad-x` adds some small horizontal
%   space on both sides to improve optical integration within text.
#(define-markup-command (wml layout props arg)
   (markup?)
   (interpret-markup layout props
                     (make-pad-x-markup
                      0.1
                      (make-center-align-markup
                       (make-with-true-dimensions-markup arg)))))
