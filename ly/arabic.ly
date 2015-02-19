\version "2.17.6"
\language "italiano"

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
  \once \override Voice.Accidental.stencil = #(lambda (grob)
  (ly:stencil-combine-at-edge
    (ly:accidental-interface::print grob) Y UP
    (grob-interpret-markup grob (markup #:line
          (#:fontsize -1 (#:musicglyph "flags.ugrace")))) -1.3))
}

%
% Arabic maqam groups
%

bayati = #`(
    (0 . ,NATURAL)
    (1 . ,SEMI-FLAT)
    (2 . ,FLAT)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,FLAT)
    (6 . ,FLAT)
  )

kurd = #`(
    (0 . ,NATURAL)
    (1 . ,FLAT)
    (2 . ,FLAT)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,FLAT)
    (6 . ,FLAT)
    )

rast = #`(
    (0 . ,NATURAL)
    (1 . ,NATURAL)
    (2 . ,SEMI-FLAT)
    (3 . ,NATURAL)
    (4 . ,NATURAL)
    (5 . ,NATURAL)
    (6 . ,SEMI-FLAT)
  )

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
         (3 . ,SEMI-SHARP) (0 . ,SEMI-SHARP) (4 . ,SEMI-SHARP) (1 . ,SEMI-SHARP)
         (5 . ,SEMI-SHARP) (2 . ,SEMI-SHARP) (6 . ,SEMI-SHARP)
         (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT ) (1 . ,DOUBLE-FLAT)
         (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
         (3 . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (1 . ,DOUBLE-SHARP)
         (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
        )
  }
}
