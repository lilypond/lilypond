\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests some nasty Gourlay spacings";
}

\version "0.1.9";

%{

This is taken from [Gourlay]'s paper on breaking lines

%}

\score{
       \melodic \multi 2 < \octave c'';
               { \stemup; d2 d     | d d | d4 d2. | }
               \octave c';
              { \stemdown; g4 g g g | \[2/3 g2 g2 g2 \] | g4. g8 g2 | }
               >
       \paper{
           linewidth = 4.\cm;
       }
}

