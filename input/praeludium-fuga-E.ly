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

\version "1.0.7";



praeludium_commands = \notes {
  \time 4/4;
   \key e;
}

praeludium_right =  \notes {
  \$praeludium_commands
  \clef violin;

  % 13 -- how to type -- where to split -- this more neatly?
  \type Staff <
    \type Voice = I \relative c'' { \stemup r4 dis4 e4. e8 ~ |
      \shifton e4 [d8 fis8] \shiftoff gis4 ~ [gis8 fis16 e ] |
      fis4 ~ [fis8 e16 dis] e4 r8 e8 }
    \type Voice = III \relative c'' { \stemup \shifton r4 bis cis \shiftoff cis |
      a' ~ [a16 gis a b] \shifton dis,4 cis ~ |
      [cis8 dis16 ais] bis4 cis r8 b }
    \type Voice = IV \relative c'' {

      %\stemup
      %{
      this is a diversion from the Griepenkerl/Keller
       edition; a hack to avoid collisions
      %}
      \stemdown
      \shifton s4 gis }
      
    \type Voice =  II \relative c' { \stemdown
%      \shifton       % idem

      r4 fis \shiftoff gis gis |
      a4. cis8 gis2 |
      fis4 gis gis r8 e8 }
  > |
  % 16
}

praeludium_left = \notes \relative c {
  \$praeludium_commands
  \clef bass;

  % 13
  \type Staff <
    { \stemup r4 dis' cis cis ~ |
      [cis8 a d cis] [bis gis] cis4 |
      dis2 cis4 r8 cis }
    { \stemup bis2 }
    { \stemup \shifton r4 gis ~ [gis8 gis] ~ \stemdown \shiftoff gis4 |
      a4. fis8 gis4. a8 ~ |
      a4 gis4 gis r8 gis }
%    { \stemup \shifton s4 fis4 e}
% a quick hack to avoid some collisons
    { \stemdown \shifton s4 fis4 e}
    { \stemdown s4 dis4 cis4 }
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
  \key e;              % E-major
}

fugaII_right = \notes   \relative c''   {
  \$fugaII_commands
  \clef violin;

  %15
  \type Staff <
    { \stemup [b8 fis8] b4 }
    { \stemdown fis2 }
  >
   %{ this chord is usually set like this:
        |
       x|| 
        x||
         x|
      |x
      |
   %}
   \type Staff <
     { \stemup \shiftoff e4 }
     { \stemup \shifton cis }
     { \stemup \shifton ais }
     { \stemdown fis }
   > |
  %16
  \type Staff <
    { \stemup dis2 dis4 |
      cis2 cis4 |
      b4. [cis8 dis e] }
    { \stemup \shifton [b8 fis] b2 ~ |
      [b8 a!16 gis] a2 ~ |
      a4 gis2 }
    { \stemdown fis2. ~ |
      fis ~ |
      fis4 e2 }
  > |
  %19
}

fugaII_left = \notes {
  \$fugaII_commands  
  \clef bass;

  %15
  \type Staff < 
    { \stemdown b2 \stemup ais4 |
      b2 b4 }
    { \stemdown s2 e4 |
      fis2 fis4 }
  >
  \stemdown cis2 e4 |
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
  \type Score <
    \type GrandStaff <
      \type Staff = treble {
        \praeludium_right \breakmusic \fugaII_right }
      \type Staff = bass { 
        \praeludium_left \breakmusic \fugaII_left }
    > 
    \type Staff = pedal {
      \praeludium_pedal \breakmusic \fugaII_pedal }
  >

  \paper{}

  \midi {
    \tempo 4 =96; }
}
