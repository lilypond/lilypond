\header {
    title	= "ancient font test";
    date	= "2000";
}

\version "1.3.117";

\include "paper26.ly"

global =  \notes {
    \property Score.timing = 1
    \property Staff.TimeSignature \override #'style = #"old"
    \time 2/2;
}

upperVoice =  \context Staff = upperVoice <
    \global
    \property Staff.numberOfStaffLines = 4
    \notes \transpose c' {
	\property Staff.Custos \override #'style = #'"vaticana"
	\clef "vaticana_fa2";
	c1 d e f g
	\clef "vaticana_do2";
	a b c' \bar "|";
	b a g f
	\clef "vaticana_fa1";
	e d c\breve \bar "|";

	\property Staff.Custos \override #'style = #"medicaea"
	\clef "medicaea_fa2";
	c1 d e f g
	\clef "medicaea_do2";
	a b c' \bar "|";
	b a g f
	\clef "medicaea_fa1";
	e d c\breve \bar "|";

	\property Staff.Custos \override #'style = #'"hufnagel"
	\clef "hufnagel_fa2";
	c1 d e f g
	\clef "hufnagel_do2";
	a b c' \bar "|";
	b a g f
	\clef "hufnagel_fa1";
	e d c\breve \bar "||";
    }
>

lowerVoice =  \context Staff = lowerNotes <
    \global
    \property Staff.numberOfStaffLines = 5
    \notes \transpose c'' {
	\property Staff.Custos \override #'style = #'"mensural"
	\clef "mensural1_c2";
	c1 d e f g
	\clef "mensural1_c2";
	a b c' \bar "|";
	b a g f
	\clef "mensural2_c2";
	e d c\breve \bar "|";

	\clef "mensural2_c2";
	c1 d e f g
	\clef "mensural3_c2";
	a b c' \bar "|";
	b a g f
	\clef "mensural3_c2";
	e d c\breve \bar "|";

	\clef "mensural_f";
	c1 d e f g
	\clef "mensural_f";
	a b c' \bar "|";
	b a g f
	\clef "mensural_f";
	e d c\breve \bar "|";

	\clef "hufnagel";
	c,1 d, e, f, g,
	\clef "hufnagel";
	a, b, c \bar "||";
    }
>

\score {
    \context ChoirStaff <
	\upperVoice
	\lowerVoice
    >
    \paper {
%	\paper_twentysix
	linewidth = 17.25\cm;
	textheight = 26.0\cm;
	indent = 0.0;
	\translator {
	    \StaffContext
	    \consists "Custos_engraver";
%	    custosStyle = "mensural";
	}
    }
}

