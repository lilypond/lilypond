\header{
filename =	 "viola-part.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\include "global-i.ly"
\include "viola-i.ly"

\score{
	\$viola_i_staff
	\paper{
		\translator { \BarNumberingStaffContext }
	}
	\midi{
%urg
%		\tempo 4. = 69;
		\tempo 4 . = 69;
	}
}

