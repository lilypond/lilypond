\header{
filename =	 "prelude-viola.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Pr\\'elude";		% duh
opus =	 "BWV 1008";
% opus =	 "";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

%{
 Tested Features:breaking algorithm, chords, multivoice, accents, 
 dotted slurs
%}

\version "1.0.7";

\include "prelude-urtext.ly";

prelude_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\skip 2.*63;
	\bar "|.";
}

prelude_viola_scripts = \notes{
}

prelude_viola_staff = \type Staff <
	\notes \transpose c' \$prelude
	\$prelude_viola_global
	\$prelude_viola_scripts
>

\score{
	\$prelude_viola_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 40; }
	\header{ piece = "Pr\\'elude"; }
}

