%%  This file is part of LilyPond, the GNU music typesetter.
%%  This file can replace arabic.ly for people wanting to use c d e f g a b
%%  instead of do re mi fa sol la si
%
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

\version "2.23.3"
\include "arabic.ly"
\language "english"

%% The keys

%% Souznak: c' d' edb' f' g' ab' b' c'' c'' b' ab' g' f' edb' d' c'
souznak = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Nahawande: c d eb f g ab b c c bb ab g f eb d c
nahawande = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Nawaatar: c d eb fd g ab b c c b ab g fd eb d c
nawaatar = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,SHARP)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Houssaini: d edb f g a bdb c d c bb a g f edb d c
%% Houssaini: en do: c ddb eb f g adb bb c c bb ab g f eb ddb c
houssaini =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,FLAT)
)

%% Karjkhar: d' edb' f' g' ab' b' c'' d'' d'' b' ab' g' f' edb d
%% Karjkhar: en do: c' ddb' eb' f' gb' a' bb' c''bb' a' gb' f' eb' dbd' c'
karjkhar =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,FLAT)
  (5 . ,NATURAL)
  (6 . ,FLAT)
)

%% Saba: d edb f gb a bb c d c bb a gb f edb d
%% Saba en do: c ddb eb fb g ab bb c c bb ab g fb eb ddb c
saba =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,FLAT)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%% Gammes commençant par midb

%% Hizaz Avec sib et mib: d' eb' fd' g' a' bdb' c'' d'' d'' c''bb' a' g' fd' eb' d'
%% Hizaz: en do: c db e f g adb bb c c bb ab g f e db c
hizaz = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,FLAT)
)

%% Jaharkah: f g a bb c d edb f f eb d c bb a g f
%% Jaharkah: en do : c d e f g a bdb c c bb a g f e d c
jaharkah = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)
