\header{
filename =	 "violino-violoncello.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

\include "violino-i.ly";
\include "violoncello-i.ly";

\score{
	\context GrandStaff <
		\$violino_i_staff
		\$violoncello_i_staff
	>
	\paper{
		\translator {
%			\BarNumberingStaffContext
%			\BarNumberingScoreContext
			\OrchestralScoreContext
		}
	}
	\midi{
%urg
%		\tempo 4. = 69;
		\tempo 4 . = 69;
	}
}

\version "1.0.20";
