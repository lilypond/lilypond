\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests some nasty Gourlay spacings";
}

\version "1.0.7";

%{

This is taken from [Gourlay]'s paper on breaking lines

%}

\score{
       \notes \type Staff  \transpose c''<
               { \stemup d2 d     | d d | d4 d2. | \break  c1 }
               \transpose c, { \stemdown g4 g g g | \[2/3 g2 g2 g2 \] | g4. g8 g2 | c1 }
               >
       \paper{
           linewidth = 8.\cm
       }
}

