\version "1.3.146"
\header {
texidoc="setting staff symbol properties the normal way is broken"
    title	= "ancient font test"
    date	= "2000"
}



\include "paper26.ly"

global =  \notes {
    \property Score.timing = ##f
%   \property Staff.TimeSignature \override #'style = #'old
}

upperVoice =  \context Staff = upperVoice <
    \global
    \property Staff.StaffSymbol \override #'line-count = #4
    \notes \transpose c' {
	\property Voice.NoteHead \override #'style = #'mensural
	\property Voice.Stem \override #'stem-centered = ##t
	\property Staff.Custos \override #'style = #'vaticana
	\clef "vaticana_fa2"
	c2 d e f g

%	\property Staff.clefGlyph = #"clefs-vaticana_do"
%	\property Staff.clefPosition = #1
%	\property Staff.clefOctavation = #0 
	\clef "vaticana_do2"

	a b c'
	b a g f
	\clef "vaticana_fa1"
	e d c1 \bar "|"

	\property Staff.Custos \override #'style = #'medicaea
	\clef "medicaea_fa2"
	c2 d e f g
	\clef "medicaea_do2"
	a b c'
	b a g f
	\clef "medicaea_fa1"
	e d c1 \bar "|"

	\property Staff.Custos \override #'style = #'hufnagel
	\clef "hufnagel_fa2"
	c2 d e f g
	\clef "hufnagel_do2"
	a b c'
	b a g f
	\clef "hufnagel_fa1"
	e d c1 \bar "||"
    }
>

lowerVoice =  \context Staff = lowerNotes <
    \global
    \property Staff.StaffSymbol \override #'line-count = #5
    \notes \transpose c' {
        \property Voice.NoteHead \override #'style = #'mensural
	\property Voice.Stem \override #'stem-centered = ##t
	\property Staff.Custos \override #'style = #'mensural
	\clef "mensural1_c2"
	c2 d e f g
        \property Staff.forceClef = ##t
	\clef "mensural1_c2"
	a b c'
	b a g f
	\clef "mensural2_c2"
	e d c1 \bar "|"

	\clef "mensural2_c2"
	c2 d e f g
        \property Staff.forceClef = ##t
	\clef "mensural3_c2"
	a b c'
	b a g f
	\clef "mensural3_c2"
	e d c1 \bar "|"

	\clef "mensural1_f"
	c2 d e f g
        \property Staff.forceClef = ##t
	\clef "mensural1_f"
	a b c'
	b a g f
	\clef "mensural2_f"
	e d c1 \bar "|"

        \property Staff.forceClef = ##t
	\clef "mensural2_f"
	c2 d e f g
	\clef "mensural_g"
	a' b' c''
	b' a' g' f'
        \property Staff.forceClef = ##t
	\clef "mensural_g"
	e' d' c'1 \bar "|"

        \property Staff.forceClef = ##t
	\clef "mensural_g"
	c'2 d' e' f' g'
	\clef "hufnagel_do_fa"
	a b c'
	b a g f
        \property Staff.forceClef = ##t
	\clef "hufnagel_do_fa"
	e d c1 \bar "||"
    }
>

\score {
    \context ChoirStaff <
	\upperVoice
	\lowerVoice
    >
    \paper {
%	\paperTwentysix
	linewidth = 17.25\cm
	textheight = 26.0\cm
	indent = 0.0
	\translator {
	    \StaffContext
	    \consists Custos_engraver
%	    Custos \override #'style = #'mensural
	    \remove Time_signature_engraver
%	    StaffSymbol \override #'line-count = #4
	}
    }
}

