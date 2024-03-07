%%  This file is part of LilyPond, the GNU music typesetter.
%
%%  Copyright (C) 2014--2023 Hassan EL FATIHI <hassan.elfatihi@free.fr>
%%
%%  LilyPond is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%  LilyPond is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%  You should have received a copy of the GNU General Public License
%%  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.23.14"
\include "arabic.ly"


% Define pitch names for backward compatibility.
%
%  suffix  effect
%  ----------------------------------------
%   b      flat (bemol)
%   d      sharp (diesis)
%   db     quarter-tone flat (semi-bemol)
%   dd     quarter-tone sharp (semi-diesis)
%   tqb    three-quarter-tones flat
%   tqd    three-quarter-tones sharp

helArabicPitchNames = #`(
  (c . ,(ly:make-pitch -1 0 NATURAL))
  (d . ,(ly:make-pitch -1 1 NATURAL))
  (e . ,(ly:make-pitch -1 2 NATURAL))
  (f . ,(ly:make-pitch -1 3 NATURAL))
  (g . ,(ly:make-pitch -1 4 NATURAL))
  (a . ,(ly:make-pitch -1 5 NATURAL))
  (b . ,(ly:make-pitch -1 6 NATURAL))

  (cd . ,(ly:make-pitch -1 0 SHARP))
  (dd . ,(ly:make-pitch -1 1 SHARP))
  (ed . ,(ly:make-pitch -1 2 SHARP))
  (fd . ,(ly:make-pitch -1 3 SHARP))
  (gd . ,(ly:make-pitch -1 4 SHARP))
  (ad . ,(ly:make-pitch -1 5 SHARP))
  (bd . ,(ly:make-pitch -1 6 SHARP))

  (cb . ,(ly:make-pitch -1 0 FLAT))
  (db . ,(ly:make-pitch -1 1 FLAT))
  (eb . ,(ly:make-pitch -1 2 FLAT))
  (fb . ,(ly:make-pitch -1 3 FLAT))
  (gb . ,(ly:make-pitch -1 4 FLAT))
  (ab . ,(ly:make-pitch -1 5 FLAT))
  (bb . ,(ly:make-pitch -1 6 FLAT))

  (cdd . ,(ly:make-pitch -1 0 SEMI-SHARP))
  (ddd . ,(ly:make-pitch -1 1 SEMI-SHARP))
  (edd . ,(ly:make-pitch -1 2 SEMI-SHARP))
  (fdd . ,(ly:make-pitch -1 3 SEMI-SHARP))
  (gdd . ,(ly:make-pitch -1 4 SEMI-SHARP))
  (add . ,(ly:make-pitch -1 5 SEMI-SHARP))
  (bdd . ,(ly:make-pitch -1 6 SEMI-SHARP))

  (cdb . ,(ly:make-pitch -1 0 SEMI-FLAT))
  (ddb . ,(ly:make-pitch -1 1 SEMI-FLAT))
  (edb . ,(ly:make-pitch -1 2 SEMI-FLAT))
  (fdb . ,(ly:make-pitch -1 3 SEMI-FLAT))
  (gdb . ,(ly:make-pitch -1 4 SEMI-FLAT))
  (adb . ,(ly:make-pitch -1 5 SEMI-FLAT))
  (bdb . ,(ly:make-pitch -1 6 SEMI-FLAT))

  (ctqb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
  (dtqb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
  (etqb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
  (ftqb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
  (gtqb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
  (atqb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
  (btqb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))

  (ctqd . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
  (dtqd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
  (etqd . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
  (ftqd . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
  (gtqd . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
  (atqd . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
  (btqd . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
)

% Register language 'arabic' for backward compatibility
% but don't activate it by default.

#(set! language-pitch-names
       (append language-pitch-names
               (list `(arabic . ,helArabicPitchNames))))


%% Additional Arabic tunings.

%% Souznak: c d edb f g ab b c  |  c b ab g f edb d c
souznak = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Nahawande: c d eb f g ab b c  |  c bb ab g f eb d c
nahawande = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Nawaatar: c d eb fd g ab b c  |  c b ab g fd eb d c
nawaatar = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,SHARP)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Houssaini:      d edb f g a bdb c d    |  d c bb a g f edb d c
%% Houssaini in c: c ddb eb f g adb bb c  |  c bb ab g f eb ddb c
houssaini =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,FLAT)
)

%% Karjkhar:      d edb f g ab b c d    |  d c b ab g f edb d
%% Karjkhar in c: c ddb eb f gb a bb c  |  c bb a gb f eb dbd c
karjkhar =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,FLAT)
  (5 . ,NATURAL)
  (6 . ,FLAT)
)

%% Saba:      d edb f gb a bb c d    |  d c bb a gb f edb d
%% Saba in c: c ddb eb fb g ab bb c  |  c bb ab g fb eb ddb c
saba =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,FLAT)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%% Hizaz with sib et mib: d eb fd g a bdb c d  |  d c bb a g fd eb d
%% Hizaz in c:            c db e f g adb bb c  |  c bb ab g f e db c
hizaz = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,FLAT)
)

%% Jaharkah:      f g a bb c d edb f  |  f eb d c bb a g f
%% Jaharkah in c: c d e f g a bdb c   |  c bb a g f e d c
jaharkah = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)
