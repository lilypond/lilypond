\header{
filename =	 "fagotti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

\include "fagotto-1.ly"
\include "fagotto-2.ly"

$fagotti_staff = \type Staff = fagotti <
	\property Staff.instrument = "bassoon"
	\clef "bass";
	\melodic< 
		\global;
		\$fagotto1
		\$fagotto2
	>
>

