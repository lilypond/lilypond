\header{
filename =	 "violoncello-part.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\include "global-i.ly"
\include "violoncello-i.ly"

\score{
	\$violoncello_i_staff
	\paper{
		\translator { \BarNumberingStaffContext }
	}
	\midi{
%urg
%		\tempo 4. = 69;
		\tempo 4 . = 69;
	}
}

