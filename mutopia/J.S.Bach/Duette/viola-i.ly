\header{
filename =	 "viola-i.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\version "1.2.0";

\include "global-i.ly"
\include "violoncello-i.ly";

$viola_i_staff = \context Staff = viola <
  \property Staff.instrument = "viola"
  %\property Staff.instrument = "violin"
  \notes\transpose c'' \$violoncello_i
  \clef alto;
  \$global_i
>
