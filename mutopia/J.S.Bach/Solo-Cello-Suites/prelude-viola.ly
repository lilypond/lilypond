


\version "1.3.93";

\include "prelude-urtext.ly";

prelude_viola_global = \notes{
	\time 3/4;
	\key f \major;
	\clef alto;
	\skip 2.*63;
	\bar "|.";
}

prelude_viola_scripts = \notes{
}

prelude_viola_staff = \context Staff <
	\notes \transpose c' \$prelude
	\$prelude_viola_global
	\$prelude_viola_scripts
>

\score{
	\$prelude_viola_staff
	\paper{ }
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Pr\\'elude"; }
}

