\version "2.3.2"
% TODO: split ancient-font into seperate files; possibly in
% different locations.
\header {
texidoc = "@cindex Ancient Font
Here are shown many (all?) of the symbols that are
included in LilyPond's support of ancient notation.
"
}


upperStaff = \context GregorianStaff = upperStaff <<
  \context GregorianVoice <<
    \set Score.timing = ##f
%   \set Score.forceAccidental = ##t %%%%%%%% FIXME: what happened to this property?

    \override Staff.StaffSymbol  #'line-count = #4

    \notes \transpose c c {
	\override Staff.KeySignature  #'style = #'vaticana
	\override Staff.Accidental  #'style = #'vaticana
	\override NoteHead  #'style = #'vaticana_punctum
	\key es \major
	\clef "vaticana_fa2"
	c!1 des! e! f! ges!

	\override NoteHead  #'style = #'vaticana_inclinatum
	a! b! ces'
	\override Staff.BarLine  #'bar-size = #3.0 \bar "|"
%	\break % 1 (8*1)

	\override NoteHead  #'style = #'vaticana_quilisma
	b! des'! ges! fes!
	\breathe
	\clef "vaticana_fa1"
	\override NoteHead  #'style = #'vaticana_plica
	es d
	\override NoteHead  #'style = #'vaticana_reverse_plica
	c d
	\override Staff.BarLine  #'bar-size = #3.0 \bar "|"
%	\break %2 (8*1)

	\override NoteHead  #'style = #'vaticana_punctum_cavum
	es f
	\override NoteHead  #'style = #'vaticana_lpes
	g as
	\override NoteHead  #'style = #'vaticana_upes
	bes as
	\override NoteHead  #'style = #'vaticana_vupes
	g f
	\override NoteHead  #'style = #'vaticana_linea_punctum
	\override Staff.BarLine  #'bar-size = #2.0 \bar "|"
%	\break % 3 (8*1)

	es d
	\override NoteHead  #'style = #'vaticana_epiphonus
	c d
	\override NoteHead  #'style = #'vaticana_cephalicus
	es f

	\override Staff.KeySignature  #'style = #'medicaea
	\override Staff.Accidental  #'style = #'medicaea
	\override Staff.Custos  #'style = #'medicaea
	\override NoteHead  #'style = #'medicaea_punctum
	\clef "medicaea_fa2"
	ces! des!
	\override Staff.BarLine  #'bar-size = #3.0 \bar "|"
%	\break % 4 (8*1)

	e! f! ges!
	\clef "medicaea_do2"
	\override NoteHead  #'style = #'medicaea_inclinatum
	a! b! ces'!
	\override NoteHead  #'style = #'medicaea_virga
	b! a!
	\override Staff.BarLine  #'bar-size = #3.0 \bar "|"
%	\break % 5 (8*1)

	ges! fes!
	\clef "medicaea_fa1"
	\override NoteHead  #'style = #'medicaea_rvirga
	e! des! ces!

	\override Staff.KeySignature  #'style = #'hufnagel
	\override Staff.Accidental  #'style = #'hufnagel
	\override Staff.Custos  #'style = #'hufnagel
	\override NoteHead  #'style = #'hufnagel_punctum
	\clef "hufnagel_fa2"
	ces! des! es!
	\override Staff.BarLine  #'bar-size = #3.0 \bar "|"
%	\break % 6 (8*1)

	fes! ges!
	\clef "hufnagel_do2"
	\override NoteHead  #'style = #'hufnagel_lpes
	as! bes! ces'!
	\override NoteHead  #'style = #'hufnagel_virga
	bes! as!
	\override Staff.BarLine  #'bar-size = #3.0 \bar "|"
%	\break % 7 (8*1)

	ges! fes!
	\clef "hufnagel_do_fa"
	\override NoteHead  #'style = #'hufnagel_punctum
	es! des! ces! des! es! fes!
	\bar "||"
%	\break % 8 (8*1)

	s32*1
%	\break % 12 (32*1)
    }
  >>
>>

lowerStaff = \context MensuralStaff = lowerStaff <<
  \context MensuralVoice <<
    
    % this is broken until further notice -- see refman
    % \override Staff.StaffSymbol  #'line-count = #5
    \context Staff \applyoutput #(outputproperty-compatibility (make-type-checker 'staff-symbol-interface) 'line-count 5)

    \notes \transpose c c {
	\set autoBeaming = ##f
	\override NoteHead  #'style = #'neo_mensural
	\override Rest  #'style = #'neo_mensural
	\key a \major

% FIXME: lily crashes on some (invalid?) ligatures with:
%   ERROR: In procedure gh_scm2int:
%   ERROR: Wrong type argument in position 1: ()

% FIXME: lily emits "Programming error: Infinity or NaN encountered"
% on many ligatures such as BB.

	cis'1 d'\breve gis'\breve e'\breve \[ e'\longa fis'\longa \]
	\set Staff.forceClef = ##t
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
	\set Staff.forceClef = ##t
	\clef "mensural_c2"
	r\breve
	\bar "|"
%	\break % 5 (8*1)

	r2
	\clef "mensural_g"
	r4 r8 r16 r16
	\override NoteHead  #'style = #'mensural
	\override Stem  #'flag-style = #'mensural
	\override Stem  #'thickness = #1.0
	\override Rest  #'style = #'mensural
	\clef "petrucci_f"
	c8 b, c16 b, c32 b, c64 b, c64 b,
	d8 e  d16 e  d32 e  d64 e  d64 e
	r\longa
	\set Staff.forceClef = ##t
	\clef "petrucci_f"
	r\breve
	\bar "|"
%	\break % 6 (8*1)

	r\breve 
	\clef "mensural_f"
	% FIXME: must set Stem flag-style to #'neo_mensural to avoid
	% segmentation fault on r8/r16/r32.  (Strange: what has
	% Stem flag-style to do with mensural rests?)
	\override Stem  #'flag-style = #'neo_mensural
	% FIXME: produces warnings about "flag `neo_mensurald4' (or 3) not found".
	r2 r4 r8 r16 r16
	\override Stem  #'flag-style = #'mensural
	\set Staff.forceClef = ##t
	\clef "mensural_f"
	e\breve f g a1
	\clef "mensural_g"
%	\break % 7 (8*1)

	\[ bes'!\longa a'!\longa c''!\longa \]
	e'1 d' c' d' \bar "|"
	\bar "|"
%	\break % 9 (16*1)

	bes'!\longa fis'!1 as'!1 ges'!\longa % lig
	\set Staff.forceClef = ##t
	\clef "mensural_g"
	e'2 d' c' \bar "|"
%	\break % 11 (16*1)

	\set Staff.forceClef = ##t
	\clef "petrucci_g"
	c'2 d' e' f'
	\clef "petrucci_g"
	g' as'! bes'! cis''!
	bes'! as'! gis'! fis'!
	\set Staff.forceClef = ##t
	\clef "mensural_g"
	es'! des'! cis'!1 \bar "||"
%	\break % 12 (8*1)
    }
  >>
>>

\score {
    \context Score <<
	\upperStaff
	\lowerStaff
    >>
    \paper {
% do we want to keep these settings? -gp
	linewidth = 17.25\cm
	textheight = 26.0\cm
	linethickness = #(/ staffspace 5.0)
	indent = 0.0
	\context {
	    \Score
	    \accepts MensuralStaff
	    \accepts GregorianStaff
%	    timing = ##f %%%%%%%% FIXME: this has no effect
	}
	\context {
	    \Voice
	    \name MensuralVoice
	    \alias Voice
	    \remove Ligature_bracket_engraver
	    \consists Mensural_ligature_engraver
	    \override NoteHead #'style = #'mensural
%	    \override Stem #'flag-style = #'mensural %%%%%%%% FIXME: this core dumps
	    \override Stem #'thickness = #1.0
	    \override Rest #'style = #'mensural
	    autoBeaming = ##f
	}
	\context {
	    \Voice
	    \name GregorianVoice
	    \alias Voice
	    \remove Ligature_bracket_engraver
%	    \consists Gregorian_ligature_engraver %%%%%%%% TODO: not yet implemented
	    \override NoteHead #'style = #'vaticana_punctum
	    autoBeaming = ##f
	}
	\context {
	    \Staff
	    \name MensuralStaff
	    \alias Staff
	    \accepts MensuralVoice
	    \consists Custos_engraver
	    \override TimeSignature #'style = #'mensural
	    \override KeySignature #'style = #'mensural
	    \override Accidental #'style = #'mensural
	    \override Custos #'style = #'mensural
	    \override Custos #'neutral-position = #3
	    \override Custos #'neutral-direction = #-1
	    clefGlyph = #"clefs-petrucci_c2"
	    clefPosition = #-2
	    clefOctavation = #0 
	}
	\context {
	    \Staff
	    \name GregorianStaff
	    \alias Staff
	    \accepts GregorianVoice
	    \consists Custos_engraver
	    \remove Time_signature_engraver
	    \override StaffSymbol #'thickness = #2.0
	    \override StaffSymbol #'line-count = #4
	    \override KeySignature #'style = #'vaticana
	    \override Accidental #'style = #'vaticana
	    \override Custos #'style = #'vaticana
	    \override Custos #'neutral-position = #4
	    \override Custos #'neutral-direction = #-1
	    clefGlyph = #"clefs-vaticana_do"
	    clefPosition = #1
	    clefOctavation = #0 
	}
	\context {
	    \RemoveEmptyStaffContext
	    \accepts MensuralVoice
	    \accepts GregorianVoice
        }
    }
}

