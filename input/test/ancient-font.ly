\version "1.3.146"
\header {
    title	= "ancient font test"
    date	= "2000"
}



\include "paper26.ly"

global =  \notes {
    \property Score.timing = ##f
%   \property Score.forceAccidental = ##t
%   \property Staff.TimeSignature \override #'style = #'old
}

upperVoice =  \context Staff = upperVoice <
    \global
    
    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #4
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #4

    \notes \transpose c' {
	\property Staff.KeySignature \override #'style = #'vaticana
	\property Staff.Accidentals \override #'style = #'vaticana
	\property Staff.Custos \override #'style = #'vaticana
	\property Voice.NoteHead \override #'style = #'vaticana_punctum
	\key es \major
	\clef "vaticana_fa2"
	c!1 des! e! f! ges!

%	\property Staff.clefGlyph = #"clefs-vaticana_do"
%	\property Staff.clefPosition = #1
%	\property Staff.clefOctavation = #0 
	\clef "vaticana_do2"

	\property Voice.NoteHead \override #'style = #'vaticana_subbipunctum
	a! b! ces'
	\property Voice.NoteHead \override #'style = #'vaticana_virga
	b! a! ges fes
	\clef "vaticana_fa1"
	\property Voice.NoteHead \override #'style = #'vaticana_quilisma
	es d
	\property Voice.NoteHead \override #'style = #'vaticana_rvirga
	c d
	\breathe
	\property Voice.NoteHead \override #'style = #'vaticana_rvirga
	es f
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	\property Voice.NoteHead \override #'style = #'vaticana_lpes
	g as
	\property Staff.BarLine \override #'bar-size = #2.0 \bar "|"
	\property Voice.NoteHead \override #'style = #'vaticana_upes
	bes as
	\breathe
	\property Voice.NoteHead \override #'style = #'vaticana_vupes
	g f
	\property Voice.NoteHead \override #'style = #'vaticana_plica
	\property Staff.BarLine \override #'bar-size = #2.0 \bar "|"
	es d
	\property Voice.NoteHead \override #'style = #'vaticana_epiphonus
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	c d
	\property Voice.NoteHead \override #'style = #'vaticana_cephalicus
	es f

	\property Staff.KeySignature \override #'style = #'medicaea
	\property Staff.Accidentals \override #'style = #'medicaea
	\property Staff.Custos \override #'style = #'medicaea
	\property Voice.NoteHead \override #'style = #'medicaea_punctum
	\clef "medicaea_fa2"
	ces! des! e! f! ges!
	\clef "medicaea_do2"
	\property Voice.NoteHead \override #'style = #'medicaea_subbipunctum
	a! b! ces'!
	\property Voice.NoteHead \override #'style = #'medicaea_virga
	b! a! ges! fes!
	\clef "medicaea_fa1"
	\property Voice.NoteHead \override #'style = #'medicaea_rvirga
	e! des! ces! \bar "|"

	\property Staff.KeySignature \override #'style = #'hufnagel
	\property Staff.Accidentals \override #'style = #'hufnagel
	\property Staff.Custos \override #'style = #'hufnagel
	\property Voice.NoteHead \override #'style = #'hufnagel_punctum
	\clef "hufnagel_fa2"
	ces! des! es! fes! ges!
	\clef "hufnagel_do2"
	\property Voice.NoteHead \override #'style = #'hufnagel_lpes
	as! bes! ces'!
	\property Voice.NoteHead \override #'style = #'hufnagel_virga
	bes! as! ges! fes!
	\clef "hufnagel_do_fa"
	\property Voice.NoteHead \override #'style = #'hufnagel_punctum
	es! des! ces!1 \bar "||"
    }
>

lowerVoice =  \context Staff = lowerNotes <
    \global
    
    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #5
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #5
    
    \notes \transpose c' {
	\property Voice.noAutoBeaming = ##t
	\property Staff.KeySignature \override #'style = #'mensural
	\property Staff.Accidentals \override #'style = #'mensural
	\property Staff.Custos \override #'style = #'mensural
        \property Voice.NoteHead \override #'style = #'neo_mensural
	\key a \major
	\clef "neo_mensural_c2"
	c2 dis es fis ges
        \property Staff.forceClef = ##t
	\clef "neo_mensural_c2"
	ais bes cis'
	bis as gis fes
	\clef "petrucci_c2"
	e d c1 \bar "|"

	\clef "petrucci_c2"
	c2 d e f g
        \property Staff.forceClef = ##t
	\clef "mensural_c2"
	a b c'
	b a g f
	\clef "mensural_g"
	e d c1 \bar "|"

        \property Voice.NoteHead \override #'style = #'mensural
	\property Voice.Stem \override #'style = #'mensural
	\property Voice.Stem \override #'thickness = #1.0
	\clef "petrucci_f"
	c8 b, c16 b, c32 b, c64 b, c b,
	d8 e  d16 e  d32 e  d64 e  d e
	c2 d e f g
        \property Staff.forceClef = ##t
	\clef "petrucci_f"
	a b c'
	b a g f
	\clef "mensural_f"
	e d c1 \bar "|"

        \property Staff.forceClef = ##t
	\clef "mensural_f"
	c2 d e f g
	\clef "mensural_g"
	as'! bes'! cis''!
	bes'! as'! gis'! fis'!
        \property Staff.forceClef = ##t
	\clef "mensural_g"
	e' d' c'1 \bar "|"

        \property Staff.forceClef = ##t
	\clef "petrucci_g"
	c'2 d' e' f' g'
	\clef "petrucci_g"
	as'! bes'! cis''!
	bes'! as'! gis'! fis'!
        \property Staff.forceClef = ##t
	\clef "mensural_g"
	es'! des'! cis'!1 \bar "||"
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
	stafflinethickness = \staffspace / 5.0
	indent = 0.0
	\translator {
	    \StaffContext
	    \consists Custos_engraver
%	    Custos \override #'style = #'mensural
	    \remove Time_signature_engraver
%	    StaffSymbol \override #'thickness = #2.0
%	    StaffSymbol \override #'line-count = #4
	}
    }
}
