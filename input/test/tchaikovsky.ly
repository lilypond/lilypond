\version "1.7.6"
\header{
filename = 	"tchaikovsky.ly"
title = 		"Extracts from 6th symphony"
subtitle = 	"Ist movement"
composer=	"Pjotr Iljitsj Tchaikovsky"
enteredby =  	"Maarten Storm"
instrument=	"Violoncello"
}



% this is an example of extreme dynamics

% adagio mosso - 7 measures before Allegro vivo
\score{
	\notes
	  \relative c{
	 	\clef "bass"
		\key d \major
		<<a\cr d>>2-\ppp <<gis\rc\decr d'>>-\p | <<a\rced d>>2 ~ <<a d>>8 r8 r4 |
		a2-\ppp\cr a2\rc-\p\decr | <<a\rced d>>2 ~ <<a d>>8 r8 r4 |
		a2\pppp ~ a8 r8 r4 | a2\ppppp ~ a8 r8 r4 | r4 a8_"pizz"
		r8 r2^\fermata     
	}
	\header{
	piece="example 1"
	}
}

% 8 measures before Andante come prima
\score{
	\notes
	\relative c{
		\clef "bass"
		\key c \major
		<<e\sff cis'>>1^\downbow | <<cis\sff a'>>^\downbow | 
		<<d,\ffff\decr g>> | ~ <<d g>>2 ~ <<d\rced g>>8-\p 
	}
	\header{ 
	piece="example 2"
	}
}

%% new-chords-done %%
