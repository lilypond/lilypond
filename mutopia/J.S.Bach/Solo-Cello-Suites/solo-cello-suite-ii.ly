\header{
filename =	 "solo-cello-suite-ii.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
opus =	 "BWV 1008";
composer =	 "Johann Sebastian Bach (1685-1750)";
}

% urg

% \include "prelude-" + \instrument + ".ly";

i = "prelude-" + \instrument + ".ly"
ii = "allemande-" + \instrument + ".ly"
iii = "courante-" + \instrument + ".ly"
iv = "sarabande-" + \instrument + ".ly"
v = "menuetto-" + \instrument + ".ly"
vi = "gigue-" + \instrument + ".ly"

\include \i
\include \ii
\include \iii
\include \iv
\include \v
\include \vi
