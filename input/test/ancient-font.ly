\version "1.5.68"
\header {
    title	= "ancient font test"
    date	= "2002"
}

\include "paper26.ly"

upperStaff =  \context GregorianStaff = upperStaff <
  \context GregorianVoice <
    \property Score.timing = ##f
%   \property Score.forceAccidental = ##t %%%%%%%% FIXME: what happened to this property?

    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #4
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #4

    \notes \transpose c' {
	\property Staff.Clef \set #'font-family = #'ancient
	\property Staff.KeySignature \set #'font-family = #'ancient
	\property Staff.KeySignature \override #'style = #'vaticana
	\property Staff.Accidental \set #'font-family = #'ancient
	\property Staff.Accidental \override #'style = #'vaticana
	\property Voice.NoteHead \set #'font-family = #'ancient
	\property Voice.NoteHead \override #'style = #'vaticana_punctum
	\key es \major
	\clef "vaticana_fa2"
	c!1 des! e! f! ges!

	\property Voice.NoteHead \override #'style = #'vaticana_inclinatum
	a! b! ces'
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
%	\break % 1 (8*1)

	\property Voice.NoteHead \override #'style = #'vaticana_virga
	b! des'! ges! fes!
	\breathe
	\clef "vaticana_fa1"
	\property Voice.NoteHead \override #'style = #'vaticana_quilisma
	es d
	\property Voice.NoteHead \override #'style = #'vaticana_rvirga
	c d
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
%	\break %2 (8*1)

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
%	\break % 3 (8*1)

	es d
	\property Voice.NoteHead \override #'style = #'vaticana_epiphonus
	c d
	\property Voice.NoteHead \override #'style = #'vaticana_cephalicus
	es f

	\property Staff.KeySignature \override #'style = #'medicaea
	\property Staff.Accidental \override #'style = #'medicaea
	\property Staff.Custos \override #'style = #'medicaea
	\property Voice.NoteHead \override #'style = #'medicaea_punctum
	\clef "medicaea_fa2"
	ces! des!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
%	\break % 4 (8*1)

	e! f! ges!
	\clef "medicaea_do2"
	\property Voice.NoteHead \override #'style = #'medicaea_inclinatum
	a! b! ces'!
	\property Voice.NoteHead \override #'style = #'medicaea_virga
	b! a!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
%	\break % 5 (8*1)

	ges! fes!
	\clef "medicaea_fa1"
	\property Voice.NoteHead \override #'style = #'medicaea_rvirga
	e! des! ces!

	\property Staff.KeySignature \override #'style = #'hufnagel
	\property Staff.Accidental \override #'style = #'hufnagel
	\property Staff.Custos \override #'style = #'hufnagel
	\property Voice.NoteHead \override #'style = #'hufnagel_punctum
	\clef "hufnagel_fa2"
	ces! des! es!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
%	\break % 6 (8*1)

	fes! ges!
	\clef "hufnagel_do2"
	\property Voice.NoteHead \override #'style = #'hufnagel_lpes
	as! bes! ces'!
	\property Voice.NoteHead \override #'style = #'hufnagel_virga
	bes! as!
	\property Staff.BarLine \override #'bar-size = #3.0 \bar "|"
%	\break % 7 (8*1)

	ges! fes!
	\clef "hufnagel_do_fa"
	\property Voice.NoteHead \override #'style = #'hufnagel_punctum
	es! des! ces! des! es! fes!
	\bar "||"
%	\break % 8 (8*1)

	s32*1
%	\break % 12 (32*1)
    }
  >
>

lowerStaff =  \context MensuralStaff = lowerStaff <
  \context MensuralVoice <
    
    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #5
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #5

    \notes \transpose c' {
	\property Staff.Clef \set #'font-family = #'ancient
	\property Voice.autoBeaming = ##f
	\property Voice.NoteHead \set #'font-family = #'ancient
	\property Voice.NoteHead \override #'style = #'neo_mensural
	\property Voice.Stem \set #'font-family = #'ancient % ancient flags
	\property Voice.Rest \set #'font-family = #'music
	\property Voice.Rest \override #'style = #'neo_mensural
	\key a \major

% FIXME: lily crashes on some (invalid?) ligatures with:
%   ERROR: In procedure gh_scm2int:
%   ERROR: Wrong type argument in position 1: ()

% FIXME: lily emits "Programming error: Infinity or NaN encountered"
% on many ligatures such as BB.

	cis'1 d'\breve gis'\breve e'\breve \[ e'\longa fis'\longa \]
	\property Staff.forceClef = ##t
	\clef "neo_mensural_c2"
	cis1
	\bar "|"
%	\break % 2 (16*1)

	\[ g\breve dis''\longa \]
	b\breve \[ a\longa d\longa \]
	\clef "petrucci_c2"
%	\break % 4 (16*1)

	fis1 ces1
	\clef "petrucci_c2"
	r\longa
	\property Staff.forceClef = ##t
	\clef "mensural_c2"
	r\breve
	\bar "|"
%	\break % 5 (8*1)

	r2
	\clef "mensural_g"
	r4 r8 r16 r16
	\property Voice.NoteHead \override #'style = #'mensural
	\property Voice.Stem \override #'style = #'mensural
	\property Voice.Stem \override #'thickness = #1.0
	\property Voice.Rest \set #'font-family = #'ancient
	\property Voice.Rest \override #'style = #'mensural
	\clef "petrucci_f"
	c8 b, c16 b, c32 b, c64 b, c64 b,
	d8 e  d16 e  d32 e  d64 e  d64 e
	r\longa
	\property Staff.forceClef = ##t
	\clef "petrucci_f"
	r\breve
	\bar "|"
%	\break % 6 (8*1)

	r\breve 
	\clef "mensural_f"
	% FIXME: must set Voice.Stem style to #'neo_mensural to avoid
	% segmentation fault on r8/r16/r32.  (Strange: what has
	% Voice.Stem style to do with mensural rests?)
	\property Voice.Stem \override #'style = #'neo_mensural
	r2 r4 r8 r16 r32 r32
	\property Voice.Stem \override #'style = #'mensural
	\property Staff.forceClef = ##t
	\clef "mensural_f"
	e\breve f g a1
	\clef "mensural_g"
%	\break % 7 (8*1)

	\[ bes'!\longa a'!\longa c''!\longa \]
	e'1 d' c' d' \bar "|"
	\bar "|"
%	\break % 9 (16*1)

	bes'!\longa fis'!1 as'!1 ges'!\longa % lig
	\property Staff.forceClef = ##t
	\clef "mensural_g"
	e'2 d' c' \bar "|"
%	\break % 11 (16*1)

	\property Staff.forceClef = ##t
	\clef "petrucci_g"
	c'2 d' e' f'
	\clef "petrucci_g"
	g' as'! bes'! cis''!
	bes'! as'! gis'! fis'!
	\property Staff.forceClef = ##t
	\clef "mensural_g"
	es'! des'! cis'!1 \bar "||"
%	\break % 12 (8*1)
    }
  >
>

\score {
    \context Score <
	\upperStaff
	\lowerStaff
    >
    \paper {
	linewidth = 17.25\cm
	textheight = 26.0\cm
	stafflinethickness = \staffspace / 5.0
	indent = 0.0
	\translator {
	    \ScoreContext
	    \accepts MensuralStaff
	    \accepts GregorianStaff
%	    timing = ##f %%%%%%%% FIXME: this has no effect
	}
	\translator {
	    \VoiceContext
	    \name MensuralVoice
	    \alias Voice
	    \remove Ligature_bracket_engraver
	    \consists Mensural_ligature_engraver
	    NoteHead \set #'font-family = #'ancient
	    NoteHead \set #'style = #'mensural
	    Stem \set #'font-family = #'ancient
%	    Stem \set #'style = #'mensural %%%%%%%% FIXME: this core dumps
	    Stem \set #'thickness = #1.0
	    Rest \set #'font-family = #'music
	    Rest \set #'style = #'mensural
	    autoBeaming = ##f
	}
	\translator {
	    \VoiceContext
	    \name GregorianVoice
	    \alias Voice
	    \remove Ligature_bracket_engraver
%	    \consists Gregorian_ligature_engraver %%%%%%%% TODO: not yet implemented
	    NoteHead \set #'font-family = #'ancient
	    NoteHead \set #'style = #'vaticana_punctum
	    autoBeaming = ##f
	}
	\translator {
	    \StaffContext
	    \name MensuralStaff
	    \alias Staff
	    \accepts MensuralVoice
	    \consists Custos_engraver
	    TimeSignature \set #'font-family = #'ancient
	    TimeSignature \set #'style = #'mensural
	    KeySignature \set #'font-family = #'ancient
	    KeySignature \set #'style = #'mensural
	    Accidental \set #'font-family = #'ancient
	    Accidental \set #'style = #'mensural
	    Custos \set #'font-family = #'ancient
	    Custos \set #'style = #'mensural
	    Custos \set #'neutral-position = #3
	    Custos \set #'neutral-direction = #-1
	    Custos \set #'adjust-if-on-staffline = ##t
	    Clef \set #'font-family = #'ancient
	    clefGlyph = #"clefs-petrucci_c2"
	    clefPosition = #-2
	    clefOctavation = #0 
	}
	\translator {
	    \StaffContext
	    \name GregorianStaff
	    \alias Staff
	    \accepts GregorianVoice
	    \consists Custos_engraver
	    \remove Time_signature_engraver
	    StaffSymbol \set #'thickness = #2.0
	    StaffSymbol \set #'line-count = #4
	    KeySignature \set #'font-family = #'ancient
	    KeySignature \set #'style = #'vaticana
	    Accidental \set #'font-family = #'ancient
	    Accidental \set #'style = #'vaticana
	    Custos \set #'font-family = #'ancient
	    Custos \set #'style = #'vaticana
	    Custos \set #'neutral-position = #4
	    Custos \set #'neutral-direction = #-1
	    Custos \set #'adjust-if-on-staffline = ##t
	    Clef \set #'font-family = #'ancient
	    clefGlyph = #"clefs-vaticana_do"
	    clefPosition = #1
	    clefOctavation = #0 
	}
	\translator {
	    \HaraKiriStaffContext
	    \accepts MensuralVoice
	    \accepts GregorianVoice
        }
    }
}
