%%%% Definitions for writing modern Arabic music scores
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2017--2022 Amir Czwink <amir130@hotmail.de>
%%%% Copyright (C) 2008 Neil Puttock
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
\version "2.23.3"
\language "italiano" %Arabs usually use italian note names

% Modern Arabic scores are written using a 24-TET tonal system.
% The accidental that lowers by a quarter is however the slashed flat, not the
% mirrored one lilypond uses by default.
TwentyFourTETglyphs = #'(
    (0 . "accidentals.natural")
    (-1/2 . "accidentals.flat")
    (1/2 . "accidentals.sharp")

    (3/4 . "accidentals.sharp.slashslash.stemstemstem")
    (1/4 . "accidentals.sharp.slashslash.stem")
    (-1/4 . "accidentals.flat.slash")
    (-3/4 . "accidentals.mirroredflat.flat")

    (1 . "accidentals.doublesharp")
    (-1 . "accidentals.flatflat")
)

%
% Arabic maqamat ordered by maqam family
%

% Bayati family
bayati = #`(
    (0 . ,NATURAL)
    (1 . ,SEMI-FLAT)
    (2 . ,FLAT)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,FLAT)
    (6 . ,FLAT)
  )

% Hijaz family
hijaz = #`(
    (0 . ,NATURAL)
    (1 . ,FLAT)
    (2 . ,NATURAL)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,FLAT)
    (6 . ,FLAT)
)

hijaz_kar = #`(
    (0 . ,NATURAL)
    (1 . ,FLAT)
    (2 . ,NATURAL)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,FLAT)
    (6 . ,NATURAL)
)

% Kurd/Kurdi family
kurd = #`(
    (0 . ,NATURAL)
    (1 . ,FLAT)
    (2 . ,FLAT)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,FLAT)
    (6 . ,FLAT)
)

% Rast family
rast = #`(
    (0 . ,NATURAL)
    (1 . ,NATURAL)
    (2 . ,SEMI-FLAT)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,NATURAL)
    (6 . ,SEMI-FLAT)
  )

% Sikah family
sikah = #`(
    (0 . ,NATURAL)
    (1 . ,SEMI-FLAT)
    (2 . ,SEMI-FLAT)
    (3 . ,SEMI-SHARP)
    (4 . ,NATURAL)
    (5 . ,SEMI-FLAT)
    (6 . ,SEMI-FLAT)
  )

iraq = #`(
    (0 . ,NATURAL)
    (1 . ,SEMI-FLAT)
    (2 . ,SEMI-FLAT)
    (3 . ,NATURAL)
    (4 . ,SEMI-FLAT)
    (5 . ,SEMI-FLAT)
    (6 . ,SEMI-FLAT)
  )


\paper {
  font-defaults.alteration-glyph-name-alist = \TwentyFourTETglyphs
}

% Layout settings
\layout {
  \context {
    \Score
    keyAlterationOrder =
      #`(
        (6 . ,FLAT) (2 . ,FLAT) (5 . ,FLAT ) (1 . ,FLAT)
        (4 . ,FLAT) (0 . ,FLAT) (3 . ,FLAT)
        (6 . ,SEMI-FLAT) (2 . ,SEMI-FLAT) (5 . ,SEMI-FLAT ) (1 . ,SEMI-FLAT)
        (4 . ,SEMI-FLAT) (0 . ,SEMI-FLAT) (3 . ,SEMI-FLAT)
        (3 . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP)
        (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
        (3 . ,SEMI-SHARP) (0 . ,SEMI-SHARP) (4 . ,SEMI-SHARP)
        (1 . ,SEMI-SHARP)
        (5 . ,SEMI-SHARP) (2 . ,SEMI-SHARP) (6 . ,SEMI-SHARP)
        (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT )
        (1 . ,DOUBLE-FLAT)
        (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
        (3 . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP)
        (1 . ,DOUBLE-SHARP)
        (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
        )
    }
}







%%%%%%%
% Amir Czwink: I left this for backward compatibility but it is
% basically useless...
% The \dwn command is totally impractical and cumbersome, as one has to write
% the \dwn command in front of any quarter tone, and also it does not work
% for key signatures.
%
%
% Definition of "dwn" symbol in order to draw the half flat symbol
% that is more often used in Arabic music (A b with a slash across),
% rather than the reverse b symbol that is used by LilyPond.
% The method was contributed by Valentin Villenave on the LilyPond Forum
%
% http://www.mail-archive.com/lilypond-user@gnu.org/msg34244.html
%
% Exchange on 17 January 2008
%
% Updated based on email advice 10 July 2008, by Neil Puttock
%
%
% Example
%
% dod dob dosd \dwn dob dobsb dodsd do
%

dwn = {
  \once \override Voice.Accidental.stencil = #(lambda (grob)
  (ly:stencil-combine-at-edge
    (ly:accidental-interface::print grob) Y UP
    (grob-interpret-markup grob (markup #:line
          (#:fontsize -1 (#:musicglyph "flags.ugrace")))) -1.3))
}
