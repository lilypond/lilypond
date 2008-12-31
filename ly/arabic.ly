\version "2.12.0"
\include "italiano.ly"

%%%%%%%
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
  \once \override Voice.Accidental #'stencil = #(lambda (grob)
  (ly:stencil-combine-at-edge
    (ly:accidental-interface::print grob) Y UP
    (grob-interpret-markup grob (markup #:line
          (#:fontsize -1 (#:musicglyph "flags.ugrace")))) -1.3 0))
}

%
% Arabic maqam groups
%

bayati = #`(
    (0 . 0)
    (1 . ,SEMI-FLAT)
    (2 . ,FLAT)
    (3 . 0)
    (4 . 0)
    (5 . ,FLAT)
    (6 . ,FLAT)
  )

kurd = #`(
    (0 . 0)
    (1 . ,FLAT)
    (2 . ,FLAT)
    (3 . 0)
    (4 . 0)
    (5 . ,FLAT)
    (6 . ,FLAT)
    )

rast = #`(
    (0 . 0)
    (1 . 0)
    (2 . ,SEMI-FLAT)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . ,SEMI-FLAT)
  )

sikah = #`(
    (0 . 0)
    (1 . ,SEMI-FLAT)
    (2 . ,SEMI-FLAT)
    (3 . ,SEMI-SHARP)
    (4 . 0)
    (5 . ,SEMI-FLAT)
    (6 . ,SEMI-FLAT)
  )

iraq = #`(
    (0 . 0)
    (1 . ,SEMI-FLAT)
    (2 . ,SEMI-FLAT)
    (3 . 0)
    (4 . ,SEMI-FLAT)
    (5 . ,SEMI-FLAT)
    (6 . ,SEMI-FLAT)
  )

