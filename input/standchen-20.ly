% standchen-20.ly
% process using:
% 	lilypond -i symbol20.ly standchen-20

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
		% 20pt music uses whole pagewidth
		linewidth= 195.\mm;
		% 20pt music needs some space
		unitspace= 13.\mm;
	}
	\midi{
		\tempo 4 = 54;
	}
}
