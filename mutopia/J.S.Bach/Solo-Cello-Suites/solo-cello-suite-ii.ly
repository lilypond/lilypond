\include "header.ly"

\paper {
linewidth = 180.\mm;
\translator { \BarNumberingStaffContext }
}

% \include "prelude-" + \instr + ".ly";
i = "prelude-" + \instr + ".ly"
ii = "allemande-" + \instr + ".ly"
iii = "courante-" + \instr + ".ly"
iv = "sarabande-" + \instr + ".ly"
v = "menuetto-" + \instr + ".ly"
vi = "gigue-" + \instr + ".ly"

\include \i
\include \ii
\include \iii
\include \iv
\include \v
\include \vi
