\version "2.1.7"
% TODO:
% check with ancient- stuff.  rename, merge, something.  -gp

\header { texidoc = "@cindex Ancient Mensural Ligatures
LilyPond can print mensural ligatures."
}



% Note that the horizontal alignment of the fermatas obeys to the
% graphical width of the ligatures rather to the musical moment in time.
% This is intended behaviour.

voice = \notes \transpose c c' {
  \property Score.timing = ##f
  \property Score.defaultBarType = "empty"
  g\longa c\breve a\breve f\breve d'\longa^\fermata
  \bar "|"
  \[
    g\longa c\breve a\breve f\breve d'\longa^\fermata
  \]
  \bar "|"
  e1 f1 a\breve g\longa^\fermata
  \bar "|"
  \[
    e1 f1 a\breve g\longa^\fermata
  \]
  \bar "|"
  e1 f1 a\breve g\longa^\fermata
  \bar "||"
}

\score {
    \context ChoirStaff <<
	\new MensuralStaff <<
	    \context MensuralVoice <<
		\voice
	    >>
	>>
	\new Staff <<
	    \context Voice <<
		\voice
	    >>
	>>
    >>
    \paper {
	stafflinethickness = \staffspace / 5.0
	\translator {
	    \VoiceContext
	    \name MensuralVoice
	    \alias Voice
	    \remove Ligature_bracket_engraver
	    \consists Mensural_ligature_engraver
	    NoteHead \set #'style = #'mensural
	}
	\translator {
	    \StaffContext
	    \name MensuralStaff
	    \alias Staff
	    \accepts MensuralVoice
	    \consists Custos_engraver
	    TimeSignature \set #'style = #'mensural
	    KeySignature \set #'style = #'mensural
	    Accidental \set #'style = #'mensural
	    Custos \set #'style = #'mensural
	    Custos \set #'neutral-position = #3
	    Custos \set #'neutral-direction = #-1
	    Custos \set #'adjust-if-on-staffline = ##t
	    clefGlyph = #"clefs-petrucci_g"
	    clefPosition = #-2
	    clefOctavation = #-0
	}
	\translator {
	    \RemoveEmptyStaffContext
	    \accepts MensuralVoice
        }
	\translator {
	    \ScoreContext
	    \accepts MensuralStaff
	}
    }
}

