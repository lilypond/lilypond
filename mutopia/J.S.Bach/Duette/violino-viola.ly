#(set! point-and-click line-column-location)
\header{
filename = 	 "violino-viola.ly"
title = 	 	 "Vier Duette"
description = 	 "Four duets for Violino and Violoncello (Viola)"
opus =            "BWV"
composer = 	 "Johann Sebastian Bach (1685-1750)"
enteredby = 	 "jcn"
copyright = 	 "Public Domain"
}

\version "1.3.146"

% { 
\include "violino-i.ly"
\include "viola-i.ly"

\score{
  \context GrandStaff <
    \violinoIStaff
    \violaIStaff
  >
  \paper{
    \translator {
      \OrchestralScoreContext
    }
  }
  \midi{
%urg real/duration
%    \tempo 4. = 69
    \tempo 4 . = 50
  }
}

% }

\include "violino-ii.ly"
\include "viola-ii.ly"

\score{
  \context GrandStaff <
    \violinoIiStaff
    \violaIiStaff
  >
  \paper{
    \translator {
      \OrchestralScoreContext
    }
  }
  \midi{
%    \tempo 2 = 96
    \tempo 2 = 75
  }
}

