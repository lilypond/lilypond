\header {
  filename =    "praeludium-fuga-E.ly";
  title =       "praeludium and fuga in E-major";
  opus =        "BWV 566";
  description = "Praeludium 3 bar excerpt, "
                "2nd fuga transposed subject -- 4 bar excerpt.  "
                "We try to imitate the Griepenkerl/Keller edition which "
                "gives the best approximation to Bach's original layout.";
  composer =    "Johann Sebastian Bach (1685-1750)";
  enteredby =   "JCN, WL";
  copyright =   "public domain";
}

%{
 Tested Features:
 purpose of this file is testing: 
   * real-life collisions
   * multi-voice input --- splitting?
   * organ staff...
%}


\version "0.1.9";

praeludium_commands = \melodic {
  \meter 4/4;
  \key fis cis gis dis;             % E-major
}

doshift = \property Voice.hshift = 1
noshift = \property Voice.hshift = 0

praeludium_right = \melodic {
  \$praeludium_commands
  \octave c';
  \clef violin;

  % 13 -- how to type -- where to split -- this more neatly?
  \multi 2 <
    { \stemup r4 dis'4 e'4. e'8 ~ |
      \doshift e'4 [d'8 fis'8]
       \noshift gis'4 ~ [gis'8 fis'16 e'16] |
      fis'4 ~ [fis'8 e'16 dis'16] e'4 r8 e'8 }
    { \stemup  r4 bis4 cis'4 cis'4 |
      \noshift a'4 ~ [a'16 gis'16 a'16 b'16]
       \doshift dis'4 cis'4 ~ |
      [cis'8 dis'16 ais16] bis4 cis'4 r8 b8 }
    { \stemup \property Voice.hshift = 2 s4 gis4 }
    { \stemdown \property Voice.hshift = 2 r4 fis4
       \noshift gis4 gis4 |
      a4. cis'8 gis2 |
      fis4 gis4 gis4 r8 e8 }
  > |
  % 16
}

praeludium_left = \melodic {
  \$praeludium_commands
  \octave c;
  \clef bass;

  % 13
  \multi 2 <
    { \stemup r4 dis'4 cis'4 cis'4 ~ |
      [cis'8 a8 d'8 cis'8] [bis8 gis8] cis'4 |
      dis'2 cis'4 r8 cis'8 }
    { \stemup bis2 }
    { \stemup \doshift r4 gis4 ~
       [gis 8 gis8] ~ \stemdown \noshift gis4 |
      a4. fis8 gis4. a8 ~ |
      a4 gis4 gis4 r8 gis8 }
    { \stemup \property Voice.hshift = 2 s4 fis4 e}
    { \stemdown s4 dis4 cis4 }
  > |
  % 16
}

praeludium_pedal = \melodic {
  \$praeludium_commands  
  \octave c;
  \clef bass;

  % 13
  r4 'fis4-\ltoe 'e4.-\lheel e8-\rheel | 
  fis4.-\rtoe fis8-\rtoe fis4-\rtoe [e8-\ltoe a8-\rtoe] | 
  dis4-\ltoe gis4-\rtoe [cis8-\ltoe 'b!8-\lheel 'ais8-\rtoe 'gis8-\ltoe] |
  % 16
}


fuga2_commands = \melodic{
  \meter 3/4;
  \key fis cis gis dis;             % E-major
}

fuga2_right = \melodic {
  \$fuga2_commands
  \octave c';
  \clef violin;

  % 15
  \multi 2 <
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
   \multi 2 <
     { \stemup \noshift e'4 }
     { \stemup \doshift cis'4 }
     { \stemup \property Voice.hshift = 2 ais4 }
     { \stemdown fis4 }
   > |
  % 16
  \multi 2 <
    { \stemup dis'2 dis'4 |
      cis'2 cis'4 |
      b4. [cis'8 dis'8 e'8] }
    { \stemup \doshift [b8 fis8] b2 ~ |
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
  \octave c;
  \clef bass;

  % 15
  \multi 2 < 
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
  \octave c;
  \clef bass;

  % 15
  dis4.-\ltoe e8-\rtoe cis4 |
  'b4.-\lheel [cis8-\ltoe dis8-\rtoe e8-\rheel] |
  fis4.-\rtoe [e8-\rheel dis8-\rtoe cis8-\ltoe] |
  dis4-\rtoe e4-\rheel 'e4-\ltoe |
  % 19
}

break = \melodic { 
  %\meter 4/4;
  r1
}


% these should be two separate scores...
\score{
  \type Score <
    \type Grandstaff <
      \type Staff = treble {
        \praeludium_right \break \fuga2_right }
      \type Staff = bass { 
        \praeludium_left \break \fuga2_left }
    > 
    \type Staff = pedal {
      \praeludium_pedal \break \fuga2_pedal }
  >

  \paper{}

  \midi {
    \tempo 4 = 96; }
}
