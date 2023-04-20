% gregorian.ly
%%%% Gregorian layout.  This file is deprecated.
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
% Declare default layout; here for Vaticana style.  In case there will
% be additional styles, we may want to create style-specific `.ly` files
% for inclusion (e.g., `vaticana-init.ly`), move the style-dependent stuff
% over there, leave the style-independent Gregorian stuff here, and let
% the style-specific file (`vaticana-init.ly`) include this file.  The
% user then will have to include `vaticana-init.ly` instead of
% `gregorian.ly`.
%
\layout {
  indent = 0.0

  %%% TODO: should ragged-right be the default?
  %ragged-right = ##t
  ragged-last = ##t

  \context {
    \VaticanaStaff

    \override StaffSymbol.color = #red
    \override LedgerLineSpanner.color = #red
  }
  \context {
    \Score
    \remove Bar_number_engraver

    %%% FIXME: Musicologically seen, timing should be set to #f.
    %%% The question is how disruptive this change would be.
%{
    timing = ##f
    forbidBreakBetweenBarLines = ##f
%}

    \override SpacingSpanner.packed-spacing = ##t

    %%%
    %%% TODO: Play around with the following SpacingSpanner
    %%% settings to yield better spacing between ligatures.
    %%%
    %%% FIXME: setting #'spacing-increment to a small value
    %%% causes tons of "programming error: adding reverse spring,
    %%% setting to unit" messages.
    %%%
    % \override SpacingSpanner.base-shortest-duration = \musicLength 4
    % \override SpacingSpanner.shortest-duration-space = #0
    % \override SpacingSpanner.average-spacing-wishes = ##f
    % \override SpacingSpanner.spacing-increment = #0.0
    % \override SpacingSpanner.uniform-stretching = ##t
  }
}

%%% Local Variables:
%%% coding: utf-8
%%% End:
