\header{
filename =	 "violino-violoncello.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\version "1.3.4";

% {
\include "violino-i.ly";
\include "violoncello-i.ly";

\score{
  \context GrandStaff <
    \$violino_i_staff
    \$violoncello_i_staff
  >
  \paper{
    \translator {
      \OrchestralScoreContext
    }
  }
  \midi{
%urg, real/duration
%    \tempo 4. = 69;
    \tempo 4 . = 50;
  }
}

% }

\include "violino-ii.ly";
\include "violoncello-ii.ly";

\score{
  \context GrandStaff <
    \$violino_ii_staff
    \$violoncello_ii_staff
  >
  \paper{
    \translator {
      \OrchestralScoreContext
    }
  }
  \midi{
%    \tempo 2 = 96;
    \tempo 2 = 75;
  }
}

