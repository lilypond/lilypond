\version "1.5.49"
\header {
    title	= "ancient font test"
    date	= "2000"
}



\include "paper26.ly"

global =  \notes {
    \property Score.timing = ##f
%   \property Score.forceAccidental = ##t
    \property Staff.TimeSignature \set #'font-family = #'ancient
%   \property Staff.TimeSignature \override #'style = #'mensural
}

upperVoice =  \context Staff = upperVoice <
    \global
    
    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #4
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #4

    \notes \transpose c' {
	\property Staff.Clef \set #'font-family = #'ancient
	\property Staff.KeySignature \set #'font-family = #'ancient
	\property Staff.KeySignature \override #'style = #'vaticana
	\property Staff.Accidentals \set #'font-family = #'ancient
	\property Staff.Accidentals \override #'style = #'vaticana
	\property Staff.Custos \set #'font-family = #'ancient
	\property Staff.Custos \override #'style = #'vaticana
	\property Staff.Custos \override #'neutral-position = #4
	\property Staff.Custos \override #'neutral-direction = #-1
	\property Staff.Custos \override #'adjust-if-on-staffline = ##t
	\property Voice.NoteHead \set #'font-family = #'ancient
	\property Voice.NoteHead \override #'style = #'vaticana_punctum
	\property Voice.Porrectus \override #'style = #'vaticana
	\property Voice.Porrectus \override #'solid = ##t
	\property Voice.Porrectus \override #'add-stem = ##t
	\property Voice.Porrectus \override #'direction = #-1
	\property Voice.Porrectus \override #'thickness = #0.5
	\key es \major
	\clef "vaticana_fa2"
	c!1 des! e! f! ges!

%	\property Staff.clefGlyph = #"clefs-vaticana_do"
%	\property Staff.clefPosition = #1
%	\property Staff.clefOctavation = #0 
	\clef "vaticana_do2"

	\property Voice.NoteHead \override #'style = #'vaticana_inclinatum
	a! b!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	\property Voice.NoteHead \override #'style = #'vaticana_virga
	ces' b! des'! \~ ges! \~ fes!
	\breathe
	\clef "vaticana_fa1"
	\property Voice.NoteHead \override #'style = #'vaticana_quilisma
	es d
	\property Voice.NoteHead \override #'style = #'vaticana_rvirga
	c d
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	\property Voice.NoteHead \override #'style = #'vaticana_rvirga
	es f
	\property Voice.NoteHead \override #'style = #'vaticana_lpes
	g as
	\property Voice.NoteHead \override #'style = #'vaticana_upes
	bes as
	\property Voice.NoteHead \override #'style = #'vaticana_vupes
	g f
	\property Voice.NoteHead \override #'style = #'vaticana_plica
	\property Staff.BarLine \override #'bar-size = #2.0 \bar "|"
	es d
	\property Voice.NoteHead \override #'style = #'vaticana_epiphonus
	c d
	\property Voice.NoteHead \override #'style = #'vaticana_cephalicus
	es f

	\property Staff.KeySignature \override #'style = #'medicaea
	\property Staff.Accidentals \override #'style = #'medicaea
	\property Staff.Custos \override #'style = #'medicaea
	\property Voice.NoteHead \override #'style = #'medicaea_punctum
	\clef "medicaea_fa2"
	ces! des!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	e! f! ges!
	\clef "medicaea_do2"
	\property Voice.NoteHead \override #'style = #'medicaea_subbipunctum
	a! b! ces'!
	\property Voice.NoteHead \override #'style = #'medicaea_virga
	b! a!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	ges! fes!
	\clef "medicaea_fa1"
	\property Voice.NoteHead \override #'style = #'medicaea_rvirga
	e! des! ces!

	\property Staff.KeySignature \override #'style = #'hufnagel
	\property Staff.Accidentals \override #'style = #'hufnagel
	\property Staff.Custos \override #'style = #'hufnagel
	\property Voice.NoteHead \override #'style = #'hufnagel_punctum
	\clef "hufnagel_fa2"
	ces! des! es!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	fes! ges!
	\clef "hufnagel_do2"
	\property Voice.NoteHead \override #'style = #'hufnagel_lpes
	as! bes! ces'!
	\property Voice.NoteHead \override #'style = #'hufnagel_virga
	bes! as!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
	ges! fes!
	\clef "hufnagel_do_fa"
	\property Voice.NoteHead \override #'style = #'hufnagel_punctum
	es! des! ces! des! es! fes!
	\bar "||"
    }
>

lowerVoice =  \context Staff = lowerNotes <
    \global
    
    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #5
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #5

    \notes \transpose c' {
	\property Voice.autoBeaming = ##f
	\property Staff.Clef \set #'font-family = #'ancient
	\property Staff.KeySignature \set #'font-family = #'ancient
	\property Staff.KeySignature \override #'style = #'mensural
	\property Staff.Accidentals \set #'font-family = #'ancient
	\property Staff.Accidentals \override #'style = #'mensural
	\property Staff.Custos \set #'font-family = #'ancient
	\property Staff.Custos \override #'style = #'mensural
	\property Staff.Custos \override #'neutral-position = #3
	\property Staff.Custos \override #'neutral-direction = #-1
	\property Staff.Custos \override #'adjust-if-on-staffline = ##t
	\property Voice.NoteHead \set #'font-family = #'ancient
	\property Voice.NoteHead \override #'style = #'neo_mensural
	\property Voice.Stem \set #'font-family = #'ancient % ancient flags
	\property Voice.Rest \set #'font-family = #'music
	\property Voice.Rest \override #'style = #'neo_mensural
	\property Voice.Porrectus \override #'style = #'mensural
	\property Voice.Porrectus \override #'solid = ##f
	\property Voice.Porrectus \override #'add-stem = ##t
	\property Voice.Porrectus \override #'stem-direction = #1
	\property Voice.Porrectus \override #'thickness = #0.7
%	\property Voice.Porrectus \override #'auto-properties = ##t
%	\property Voice.Porrectus \override #'width = #3.0
	\key a \major

	% IMPORTANT NOTE:
	%
	% The porrectus syntax is definitely subject to change.  For
	% proper use, it may eventually change into something like this:
	%
	% \startLigature e \~ c \endLigature
	%
	% The reason is that there needs to be some enclosing instance
	% for correct handling of line breaking, alignment with
	% adjacent note heads, and placement of accidentals.

	\clef "neo_mensural_c2"
	cis' e' \~ d' gis' \~ e'
	\property Staff.forceClef = ##t
	\clef "neo_mensural_c2"

	fis' \~ b cis''
	b \~ a a \~ fis
	\clef "petrucci_c2"
	cis \~ fis ces1 % \bar "|"

	\clef "petrucci_c2"
	r\longa
	\property Staff.forceClef = ##t
	\clef "mensural_c2"
	r\breve r1 r2
	\clef "mensural_g"
	r4 r8 r16 r16 \bar "|"

	\property Voice.NoteHead \override #'style = #'mensural
	\property Voice.Stem \override #'style = #'mensural
	\property Voice.Stem \override #'thickness = #1.0
	\property Voice.Rest \set #'font-family = #'ancient
	\property Voice.Rest \override #'style = #'mensural
	\clef "petrucci_f"
	c8 b, c16 b, c32 b, c64 b, c b,
	d8 e  d16 e  d32 e  d64 e  d e
	r\longa
	\property Staff.forceClef = ##t
	\clef "petrucci_f"
	r\breve r1 % \bar "|"
	\clef "mensural_f"

	% FIXME: must set Voice.Stem style to #'neo_mensural to avoid
	% segmentation fault on r8/r16/r32.  (Strange: what has
	% Voice.Stem style to do with mensural rests?)
	\property Voice.Stem \override #'style = #'neo_mensural
	r2 r4 r8 r16 r32 r32
	\property Voice.Stem \override #'style = #'mensural

	\property Staff.forceClef = ##t
	\clef "mensural_f"
	e2 f g
	\clef "mensural_g"

	bes'! \~ as'! \~ cis''!
	bes'! \~ fis'! as'! \~ ges'!
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
	\translator {
	    \ScoreContext
%	    \remove System_start_delimiter_engraver
%	    systemStartDelimiter \override #'glyph = #'brace
	}
	\translator {
	    \ChoirStaffContext
	    \remove System_start_delimiter_engraver
%	    systemStartDelimiter \override #'glyph = #'empty
	}
    }
}
