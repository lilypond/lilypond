\header{
filename =	 "timpani.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

timpani = \melodic{
	\octave c;	
	R1 *2 | c4-.\ff r r2 | R1 *3 |
	c4-. r r2 | R1 *3 |
	c4-. r r2 | r1 |
	'g4-. r r2 | 'g4-. r r2 |
	R1 *5 |
	r2 r4 'g4-. |
}

$timpani_staff = \type Staff = timpani <
	\property Staff.instrument = "timpani"
	\clef "bass";
	\melodic< 
		\global;
		\$timpani
	>
>

