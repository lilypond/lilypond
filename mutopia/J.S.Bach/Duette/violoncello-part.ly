\header{
filename =	 "violoncello-part.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\version "1.1.66";

% {

\include "global-i.ly"
\include "violoncello-i.ly"

\score{
  \$violoncello_i_staff
  \paper{
    \translator { \BarNumberingStaffContext }
  }
  \midi{
%urg, real/duration
%    \tempo 4. = 69;
    \tempo 4 . = 69;
  }
}

% }

\include "global-ii.ly"
\include "violoncello-ii.ly"

\score{
  \$violoncello_ii_staff
  \paper{
    %castingalgorithm=0.;
    \translator { \BarNumberingStaffContext }
  }
  \midi{
%    \tempo 2 = 96;
    \tempo 2 = 75;
  }
}

