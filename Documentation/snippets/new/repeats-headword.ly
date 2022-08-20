\version "2.23.11"

\header {
  lsrtags = "headword"

  texidoc = "
Repeats headword
"

  doctitle = "Repeats headword"
}


%% http://lsr.di.unimi.it/LSR/Item?id=821
%% see also http://www.lilypond.org/doc/v2.18/Documentation/notation/repeats

% Beethoven, Op. 57
% Piano sonata 23 - Dem Grafen Franz von Brunswick Gewidmet
% Movt II, Andante con moto
% Measures 9 - 16

\include "english.ly"

\new PianoStaff <<

   \new Staff = RH {
      \clef bass
      \key df \major
      \time 2/4
      \set Score.currentBarNumber = #9
      \bar ".|:"
      <af ef c>4 (
      <af gf c>8..
      <af ef c>32
      |
      <af f df>8. )
      <df' af f>16 (
      <c' af gf>8
      <df' af f>8 )
      |
      <af ef c>4 (
      <af gf c>8..
      <af ef c>32
      |
      <af f df>8. )
      \slurDown
      <f' af f>16 (
      <<
         {
            \voiceTwo
            gf8
            f8 )
         }
         \new Voice {
            \voiceOne
            <f' af>16
            ef'16
            <df' af>8
         }
      >>
      \oneVoice
      |
      <af ef c>4 (
      <af gf c>8..
      <af ef c>32
      \clef treble
      |
      <af' df' af>8. )
      <af' f' df'>16 (
      <af' ef' c'>16
      gf'16
      <f' df' af>8 )
      |
      \slurUp
      <ef' bf gf>4
      \change Staff = LH
      \voiceOne
      <af ef c>4
      |
      <df' f df>4.
      \change Staff = RH
      \oneVoice
      r8
      \clef bass
      \bar ":|."
   }

   \new Staff = LH {
      \clef bass
      \key df \major
      \time 2/4
      <af, gf,>4 (
      <af, ef,>8..
      <af, gf,>32
      |
      <af, f,>8. )
      <af, df,>16 (
      <af, ef,>8
      <af, df,>8 )
      |
      <af, gf,>4 (
      <af, ef,>8..
      <af, gf,>32
      |
      <af, f,>8. )
      <af, df,>16 (
      <af, c,>8
      <af, df,> )
      |
      <af, gf,>4 (
      <af, ef,>8..
      <af, gf,>32
      |
      <f f,>8. )
      <df df,>16 (
      <ef ef,>8
      <f f,>8 )
      |
      \voiceTwo
      gf,4  (
      af,4 ~ )
      |
      af,16. [ (
      gf,32
      f,16.
      ef,32 ]
      df,8 )
      \oneVoice
      r8
   }

>>
