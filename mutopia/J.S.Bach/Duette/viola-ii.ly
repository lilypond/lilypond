\header{
filename =	 "viola-ii.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\version "1.3.4";

\include "global-ii.ly"
\include "violoncello-ii.ly";

$viola_ii_staff = \context Staff = viola <
  \property Staff.instrument = "viola"
  %\property Staff.instrument = "violin"
  \clef alto;
  \$global_ii
  \notes\transpose c'' \$violoncello_ii
>
