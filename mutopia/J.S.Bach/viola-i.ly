\header{
filename =	 "viola-i.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\include "global-i.ly"
\include "violoncello-i.ly";

$viola_i_staff = \context Staff = viola <
	\notes\transpose c'' \$violoncello_i
	\clef alto;
	\$global_i
>
\version "1.0.19";
