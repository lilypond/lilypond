\header{
filename =	 "viola-part.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\version "1.3.4";

% {

\include "global-i.ly"
\include "viola-i.ly"

\score{
  \$viola_i_staff
  \paper{
    \translator { \BarNumberingStaffContext }
  }
  \midi{
%urg, real/duration
%    \tempo 4. = 69;
    \tempo 4 . = 50;
  }
}

% }

\include "global-ii.ly"
\include "viola-ii.ly"

\score{
  \$viola_ii_staff
  \paper{
%    castingalgorithm=0.0;
    \translator { \BarNumberingStaffContext }
    %\translator { \ScoreContext skipBars = 1; }
  }
  \midi{
%    \tempo 2 = 96;
    \tempo 2 = 75;
  }
}
