\header{
filename =	"tchaikovsky.ly";
title =		"Extracts from 6th symphony";
subtitle =	"Ist movement";
composer=	"Pjotr Iljitsj Tchaikovsky";
enteredby = 	"Maarten Storm";
instrument=	"Violoncello";
}

\version "1.0.20";

% this is an example of extreme dynamics

% adagio mosso - 7 measures before Allegro vivo
\score{
	\notes
	  \relative c{
	 	\clef "bass";
		\key D;
		<a2\ppp\cr d> <gis\rc\p\decr d'> | <a2\rced d> ~ <a8 d> r8 r4 |
		a2\ppp\cr a2\rc\p\decr | <a2\rced d> ~ <a8 d> r8 r4 |
		a2\pppp ~ a8 r8 r4 | a2\ppppp ~ a8 r8 r4 | r4 a8_"pizz"
		r8 r2^\fermata     
	}
	\header{
	piece="example 1";
	}
}

% 8 measures before Andante come prima
\score{
	\notes
	\relative c{
		\clef "bass";
		\key C;
		<e1\sff cis'^\downbow> | <cis\sff a'^\downbow> | 
		<d,\ffff\decr g> ~ <d2 g> ~ <d8\p\rced g> |
	}
	\header{ 
	piece="example 2";
	}
}

