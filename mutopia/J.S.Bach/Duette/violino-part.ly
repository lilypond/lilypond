\header{
filename = 	 "violino-part.ly"
title = 	 	 "Vier Duette"
description = 	 "Four duets for Violino and Violoncello (Viola)"
opus =            "BWV"
composer = 	 "Johann Sebastian Bach (1685-1750)"
enteredby = 	 "jcn"
copyright = 	 "Public Domain"
}

\version "1.3.146"

% {

\include "global-i.ly"
\include "violino-i.ly"

\score{
  \violinoIStaff
  \paper{
    %\translator { \BarNumberingStaffContext }
  }
  \midi{
%    \tempo 4. = 69
    \tempo 4 . = 50
  }
}

% }

\include "global-ii.ly"
\include "violino-ii.ly"

\score{
  \violinoIiStaff
  \paper{
    %\translator { \BarNumberingStaffContext }
%    castingalgorith=0
  }
  \midi{
%    \tempo 2 = 96
    \tempo 2 = 75
  }
}

