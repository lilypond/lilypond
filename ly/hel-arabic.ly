%%  This file is part of LilyPond, the GNU music typesetter.
%%  This file can replace arabic.ly for people wanting to use c d e f g a b
%%  instead of do re mi fa sol la si
%
%
%%  Copyright (C) 2014--2022 Hassan EL FATIHI <hassan.elfatihi@free.fr>
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
\language "arabic"

%% set pitch names.
HelmakamGlyphs = #'(
  (1 . "accidentals.doublesharp")
  (1/4 . "accidentals.sharp.slashslash.stem")
  (3/4 . "accidentals.sharp.slashslashslash.stem")
  (5/2 . "accidentals.sharp.slashslashslash.stemstem")
  (7/2 . "accidentals.sharp.slashslash.stemstemstem")
  (1/2 . "accidentals.sharp")
  (0 . "accidentals.natural")
  (-1/4 . "accidentals.flat.slash")
  (-3/4 . "accidentals.mirroredflat.flat")
  (-5/2 . "accidentals.flatflat.slash")
  (-7/2 . "accidentals.flat.slashslash")
  (-1/2 . "accidentals.flat")
  (-1 . "accidentals.flatflat")
)

\paper {
  font-defaults.alteration-glyph-name-alist = \HelmakamGlyphs
}

%% The keys

%% Rast: c d edb f g a bdb c c bb a g f edb d c

%% This key can also be used for:
%% Irak: bdb c d edb f g a bdb
%% Rahatalarouah: bdb c d edb fd g a bdb
%% Alboustankar: bdb c d edb f gb a bdb
%% Sajakar: c dd edb f g a bdb altering the note d with dd (dis)

rast = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)

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

%% Alhizazkar: c db e f g ab b c c b ab g f e db c
alhizazkar = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Hizazkarkurdy: c db eb f g ab bb c c bb ab g f eb db c
hizazkarkurdy = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
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

%% Nakriz: c d eb fd g a bb c c bb a g fd eb d c
nakriz = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,SHARP)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,FLAT)
)

%% Bayati: d edb f g a bb c d c bb a g f edb d c
%% Bayati: en do: c' ddb' eb' f' g' ab' bb' c'' c'' bb' ab'g' f'  eb' ddb' c'
bayati = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
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

%% Kurd: d eb f g a bb c d c bb a g f eb d
%% Kurd: en do: c db eb f g ab bb c  c bb ab g f eb db c
kurd = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%%% Sahnaz: d' eb' fd' g' a' bb' cd'' d''
%%% Sahnaz: eb do: c' db' e' f' g' ab' b' c''
% Sahnaz (in D) is a transposition of Alhizazkar (in C)

%sahnaz = #`(
%  (0 . ,NATURAL)
%  (1 . ,FLAT)
%  (2 . ,NATURAL)
%  (3 . ,NATURAL)
%  (4 . ,NATURAL)
% (5 . ,FLAT)
%  (6 . ,NATURAL)
%)

%% Gammes commençant par midb

%% Huzam: edb f g ab b c d edb edb d c b ab f ebd: mi et la altérées
%% Huzam: en do: c ddb edb fdb gdd adb bdb c c bdb adb gdd fdb edb ddb c
huzam =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,SEMI-FLAT)
  (3 . ,SEMI-FLAT)
  (4 . ,SEMI-SHARP)
  (5 . ,SEMI-FLAT)
  (6 . ,SEMI-FLAT)
)

%% Sikah: edb f g a bdb d edb edb d a bb a g f edb
%% Sikah: en do: c ddb edb fdd g adb bdb c c bdb adb gdb fdd edb ddb c
sikah = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,SEMI-FLAT)
  (3 . ,SEMI-SHARP)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,SEMI-FLAT)
)
%% Yakah: g a bdb c d e fdd g g f edb d c bdb a g
%% Yakah: en do: c' ddb' edb' f' gdb' adb' bdb' c'' cdb'' bdb' adb' gdb' f' edb' ddb' c
yakah = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)

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

%% Chatarabane: g ab b c d eb fd g f eb d c bb ab g
%% Chatarabane: en do : c db e f g ab b
chatarabane = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Farahfaza: g a bb c d eb f g f eb d c bb a g
%% Farahfaza: en do: c d eb f g ab bb
farahfaza = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

\layout {
  indent = #0
  \context {
    \Score
    keyAlterationOrder = #`(
      (6 . ,FLAT) (2 . ,FLAT) (5 . ,FLAT) (1 . ,FLAT)
      (4 . ,FLAT) (0 . ,FLAT) (3 . ,FLAT)
      (6 . ,SEMI-FLAT) (2 . ,SEMI-FLAT) (5 . ,SEMI-FLAT) (1 . ,SEMI-FLAT)
      (4 . ,SEMI-FLAT) (0 . ,SEMI-FLAT) (3 . ,SEMI-FLAT)
      (3 . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP)
      (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
      (3 . ,SEMI-SHARP) (0 . ,SEMI-SHARP) (4 . ,SEMI-SHARP) (1 . ,SEMI-SHARP)
      (5 . ,SEMI-SHARP) (2 . ,SEMI-SHARP) (6 . ,SEMI-SHARP)
      (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT) (1 . ,DOUBLE-FLAT)
      (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
      (3 . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (1 . ,DOUBLE-SHARP)
      (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
    )
  }
}
