\version "1.5.68"
\header {
    title	= "mensural ligature test"
    date	= "2002"
}

\include "paper26.ly"

% Note that the horizontal alignment of the fermatas obeys to the
% graphical width of the ligatures rather to the musical moment in time.
% This is intended behaviour.

voice = \notes \transpose c'' {
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
    \context ChoirStaff <
	\context MensuralStaff = upperStaff <
	    \context MensuralVoice <
		\voice
	    >
	>
	\context Staff = lowerStaff <
	    \context Voice <
		\voice
	    >
	>
    >
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
	    \HaraKiriStaffContext
	    \accepts MensuralVoice
        }
	\translator {
	    \ScoreContext
	    \accepts MensuralStaff
	}
    }
}
