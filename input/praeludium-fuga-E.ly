\header {
  filename =    "praeludium-fuga-E.ly";
  title =       "praeludium and fuga in E-major";
  opus =        "BWV 566";
  composer =    "Johann Sebastian Bach (1685-1750)";
  enteredby =   "JCN";
  copyright =   "public domain";
}
%{
  description

  Praeludium 3 bar excerpt,
	2nd fuga transposed subject -- 4 bar excerpt. 
        We try to imitate the Griepenkerl/Keller edition which
	gives the best approximation to Bach's original layout
%}
%{
 Tested Features:
 purpose of this file is testing: 
   * real-life collisions
   * multi-voice input --- splitting?
   * organ staff...
%}

\version "1.3.93";



praeludium_commands = \notes {
  \time 4/4;
   \key e \major;
}

praeludium_right =  \notes {
  \$praeludium_commands
  \clef violin;

  % 13 -- how to type -- where to split -- this more neatly?
  \context Staff <
    \context Voice = I \relative c'' { \stemUp r4 dis4 e4. e8 ~ |
      \shiftOn e4 [d8 fis8] \shiftOff gis4 ~ [gis8 fis16 e ] |
      fis4 ~ [fis8 e16 dis] e4 r8 e8 }
    \context Voice = III \relative c'' { \stemUp \shiftOn r4 bis cis \shiftOff cis |
      a' ~ [a16 gis a b] \shiftOn dis,4 cis ~ |
      [cis8 dis16 ais] bis4 cis r8 b }
    \context Voice = IV \relative c'' {

      %\stemUp
      %{
      this is a diversion from the Griepenkerl/Keller
       edition; a hack to avoid collisions
      %}
      \stemDown
      \shiftOn s4 gis }
      
    \context Voice =  II \relative c' { \stemDown
%      \shiftOn       % idem

      r4 fis \shiftOff gis gis |
      a4. cis8 gis2 |
      fis4 gis gis r8 e8 }
  > |
  % 16
}

praeludium_left = \notes \relative c {
  \$praeludium_commands
  \clef bass;

  % 13
  \context Staff <
    \context Voice = two { r4 }
    \context Voice = one { \stemUp s4 dis' cis cis ~ |
      [cis8 a d cis] [bis gis] cis4 |
      dis2 cis4 r8 cis }
    \context Voice = one { \stemUp bis2 }
    \context Voice = three { \stemUp \shiftOn r4 gis ~ [gis8 gis] ~ \stemDown \shiftOff gis4 |
      a4. fis8 gis4. a8 ~ |
      a4 gis4 gis r8 gis }
%    { \stemUp \shiftOn s4 fis4 e}
% a quick hack to avoid some collisons
    \context Voice = four { \stemDown \shiftOn s4 fis4 e}
    \context Voice = two { \stemDown s4 dis4 cis4 }
  > |
  %16
}

praeludium_pedal = \notes \relative c{
  \$praeludium_commands  
  \clef bass;

  %13
  r4 fis,4-\ltoe e4.-\lheel e'8-\rheel | 
  fis4.-\rtoe fis8-\rtoe fis4-\rtoe [e8-\ltoe a-\rtoe] | 
  dis,4-\ltoe gis-\rtoe [cis,8-\ltoe b!-\lheel ais-\rtoe gis-\ltoe] |
  %16
}


fugaII_commands = \notes{
  \time3/4;
  \key e \major;              % E-major
}

fugaII_right = \notes   \relative c''   {
  \$fugaII_commands
  \clef violin;

  %15
  \context Staff <
    \context Voice = VA { \stemUp [b8 fis8] b4 }
    \context Voice = VB {  \stemDown fis2 }
  >

  \context Staff \notes\relative c''<
       \context Voice=one {
	  \property Voice.horizontalNoteShift=0
	  \property Voice.verticalDirection=1 
	  e4 
       }
       \context Voice=two {
	  \property Voice.verticalDirection=1 
	  \property Voice.horizontalNoteShift=1
	  cis
       }
       \context Voice=three {
	  \property Voice.horizontalNoteShift=2
	  \property Voice.verticalDirection=1 
	  ais
       }
       \context Voice=four {
	  \property Voice.verticalDirection=-1 
	  \property Voice.horizontalNoteShift=-1
	  fis
       }
  >

    %16
    \context Staff <
      \context Voice = one {  dis2 dis4 |
      cis2 cis4 |
      b4. [cis8 dis e] }
    \context Voice = three {  \stemUp \shiftOn [b8 fis] b2 ~ |
      [b8 a!16 gis] a2 ~ |
      a4 gis2 }
    \context Voice = two {  \stemDown fis2. ~ |
      fis ~ |
      fis4 e2 }
  > |
  %19
}

fugaII_left = \notes {
  \$fugaII_commands  
  \clef bass;

  %15
  \context Staff < 
    \context Voice = two { \stemDown b2 \stemUp ais4 |
      b2 b4 }
    \context Voice = two { \stemDown s2 e4 |
      fis2 fis4 }
  >
  \stemDown cis2 e4 |
  b4. b8 b4 |
  %19
}

fugaII_pedal = \notes \relative c {
  \$fugaII_commands  
  \clef bass;

  %15
  dis4.-\ltoe e8-\rtoe cis4 |
  b4.-\lheel [cis8-\ltoe dis8-\rtoe e8-\rheel] |
  fis4.-\rtoe [e8-\rheel dis8-\rtoe cis8-\ltoe] |
  dis4-\rtoe e4-\rheel e,4-\ltoe |
  %19
}

breakmusic = \notes { 
  %\time4/4;
  r1
}


% these should be two separate scores...
\score{
  \context Score <
    \context PianoStaff <
      \context Staff = treble {
 	\property Score.midiInstrument = "church organ"
        \praeludium_right \breakmusic \fugaII_right }
      \context Staff = bass { 
        \praeludium_left \breakmusic \fugaII_left }
    > 
    \context Staff = pedal {
      \praeludium_pedal \breakmusic \fugaII_pedal }
  >

  \paper {

   \translator { \OrchestralScoreContext }
  }

  \midi {
    \tempo 4 =96; }
}
