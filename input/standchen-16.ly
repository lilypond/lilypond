% standchen-16.ly

\include "standchen-part.ly"

\score{
	<	
		< 	\id "Lyric" "1";
			\tekstI
			 \tekstII
		>
		< \id "Piano" "";
			\multi 2;
			\melodic < \melodie \commands >
			\melodic < \begeleiding \commands >
		>
	>
	\paper{
		% 16pt mustn't use whole page width
		linewidth= 160.\mm;
		% 16pt doesn't need much space
		unitspace= 8.\mm;
		% we want gourlay, don't set geometric
		% geometric= 1.4;
		gourlay_maxmeasures = 9.;
	}
	\midi{
		\tempo 4 = 54;
	}
}
