\header{
enteredby      jcn
copyright      PD
TestedFeatures  This file tests some nasty Gourlay spacings
}

\version "0.1.6";

\score{
% huh?
%      \multi 2 \melodic < \octave c'';
       \melodic \multi 2 < \octave c'';
               { \stemup; d2 d     | d d | d4 d2. | }
               \octave c';
               % huh, plet broken?
%              { \stemdown; g4 g g g | \plet 2/3; g2 g2 g2 \plet 1/1; | g4. g8 g2 | }
               { \stemdown; g4 g g g | g2*2/3 g2*2/3 g2*2/3 | g4. g8 g2 | }
               >
       \paper{
           linewidth = 4.\cm;
       }
}

