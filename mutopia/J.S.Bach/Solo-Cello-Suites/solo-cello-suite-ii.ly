\include "header.ly"
% urg

\paper {
linewidth = 180.\mm;
\translator { \BarNumberingStaffContext }
}


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
