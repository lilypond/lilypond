\header{
filename =	"title.ly";
title =		"Title";
subtitle =	"Subtitle";
composer=	"Composer (xxxx-yyyy)";
opus = 		"Opus 0";
arranger =	"Arranger";
copyright = 	"public domain";
enteredby = 	"jcn";
source = 	"urtext";
piece =		"Piece I";
instrument=	"Instrument";
}

\score{
	\melodic{
		\octave relative;	
		c' d e f f e d c \break
		c d e f f e d c
	}
}

\header{ 
piece =	"Piece II"; 
opus = 		"Opus 1";
}

\score{
	\melodic{
	    f' e d c c d e f \break
	    f e d c c d e f
	}
}

