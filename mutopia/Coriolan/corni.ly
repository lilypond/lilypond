\header{
filename =	 "corni.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.14";

\include "corno-1.ly"
\include "corno-2.ly"

$corni_staff = \type Staff = corni <
	\property Staff.instrument = "french horn"
	\melodic< 
% ugh, key doesn't transpose along
%		\global
		\meter 4/4;
		\transpose a, \$corno1
		\transpose a, \$corno2
	>
>

