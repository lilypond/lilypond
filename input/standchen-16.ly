%{
16-point version of "leise flehen meine lieder"
%}

\include "standchen-part.ly"

\score{
	\themusic
	\paper{
		% 16pt mustn't use whole page width
		linewidth= 160.\mm;
		% we want gourlay, don't set geometric
		% geometric= 1.4;
		gourlay_maxmeasures = 9.;
		\output "standchen.out";
	}
	\midi{
		\tempo 4 = 54;
	}
}
