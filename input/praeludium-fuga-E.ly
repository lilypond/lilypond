\header {
  filename =    "praeludium-fuga-E.ly";
  title =       "praeludium and fuga in E-major";
  opus =        "BWV 566";
  description = "Praeludium 3 bar excerpt, "
                "2nd fuga transposed subject -- 4 bar excerpt.  "
                "We try to imitate the Griepenkerl/Keller edition which "
                "gives the best approximation to Bach's original layout.";
  composer =    "Johann Sebastian Bach (1685-1750)";
  enteredby =   "JCN";
  copyright =   "public domain";
}

%{
 Tested Features:
 purpose of this file is testing: 
   * real-life collisions
   * multi-voice input --- splitting?
   * organ staff...
%}

\version "1.0.1";



praeludium_commands = \melodic {
  \time 4/4;
   \key e;
}

praeludium_right = \melodic\transpose c'' {
  \$praeludium_commands
  \clef violin;

  % 13 -- how to type -- where to split -- this more neatly?
  \type Staff <
    { \stemup r4 dis'4 e'4. e'8 ~ |
      \shifton e'4 [d'8 fis'8] \shiftoff gis'4 ~ [gis'8 fis'16 e'16] |
      fis'4 ~ [fis'8 e'16 dis'16] e'4 r8 e'8 }
    { \stemup \shifton r4 bis4 cis'4 \shiftoff cis'4 |
      a'4 ~ [a'16 gis'16 a'16 b'16] \shifton dis'4 cis'4 ~ |
      [cis'8 dis'16 ais16] bis4 cis'4 r8 b8 }
    {

      %\stemup
      %{
      this is a diversion from the Griepenkerl/Keller
       edition; a hack to avoid collisions
      %}
      \stemdown
      \shifton s4 gis4 }
      
    { \stemdown
%      \shifton       % idem

      r4 fis4 \shiftoff gis4 gis4 |
      a4. cis'8 gis2 |
      fis4 gis4 gis4 r8 e8 }
  > |
  % 16
}

praeludium_left = \melodic {
  \$praeludium_commands
  \clef bass;

  % 13
  \type Staff <
    { \stemup r4 dis'4 cis'4 cis'4 ~ |
      [cis'8 a8 d'8 cis'8] [bis8 gis8] cis'4 |
      dis'2 cis'4 r8 cis'8 }
    { \stemup bis2 }
    { \stemup \shifton r4 gis4 ~ [gis 8 gis8] ~ \stemdown \shiftoff gis4 |
      a4. fis8 gis4. a8 ~ |
      a4 gis4 gis4 r8 gis8 }
%    { \stemup \shifton s4 fis4 e}
% a quick hack to avoid some collisons
    { \stemdown \shifton s4 fis4 e}
    { \stemdown s4 dis4 cis4 }
  > |
  % 16
}

praeludium_pedal = \melodic {
  \$praeludium_commands  
  \clef bass;

  % 13
  r4 fis,4-\ltoe e,4.-\lheel e8-\rheel | 
  fis4.-\rtoe fis8-\rtoe fis4-\rtoe [e8-\ltoe a8-\rtoe] | 
  dis4-\ltoe gis4-\rtoe [cis8-\ltoe b,!8-\lheel ais,8-\rtoe gis,8-\ltoe] |
  % 16
}


fuga2_commands = \melodic{
  \time 3/4;
  \key e;              % E-major
}

fuga2_right = \melodic\transpose c'' {
  \$fuga2_commands
  \clef violin;

  % 15
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
     { \stemup \shiftoff e'4 }
     { \stemup \shifton cis'4 }
     { \stemup \shifton ais4 }
     { \stemdown fis4 }
   > |
  % 16
  \type Staff <
    { \stemup dis'2 dis'4 |
      cis'2 cis'4 |
      b4. [cis'8 dis'8 e'8] }
    { \stemup \shifton [b8 fis8] b2 ~ |
      [b8 a!16 gis16] a2 ~ |
      a4 gis2 }
    { \stemdown fis2. ~ |
      fis2. ~ |
      fis4 e2 }
  > |
  % 19
}

fuga2_left = \melodic {
  \$fuga2_commands  
  \clef bass;

  % 15
  \type Staff < 
    { \stemdown b2 \stemup ais4 |
      b2 b4 }
    { \stemdown s2 e4 |
      fis2 fis4 }
  >
  \stemdown cis'2 e'4 |
  b4. b8 b4 |
  % 19
}

fuga2_pedal = \melodic {
  \$fuga2_commands  
  \clef bass;

  % 15
  dis4.-\ltoe e8-\rtoe cis4 |
  b,4.-\lheel [cis8-\ltoe dis8-\rtoe e8-\rheel] |
  fis4.-\rtoe [e8-\rheel dis8-\rtoe cis8-\ltoe] |
  dis4-\rtoe e4-\rheel e,4-\ltoe |
  % 19
}

breakmusic = \melodic { 
  %\time 4/4;
  r1
}


% these should be two separate scores...
\score{
  \type Score <
    \type GrandStaff <
      \type Staff = treble {
        \praeludium_right \breakmusic \fuga2_right }
      \type Staff = bass { 
        \praeludium_left \breakmusic \fuga2_left }
    > 
    \type Staff = pedal {
      \praeludium_pedal \breakmusic \fuga2_pedal }
  >

  \paper{}

  \midi {
    \tempo 4 = 96; }
}
