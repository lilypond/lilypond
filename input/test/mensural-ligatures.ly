\version "1.5.68"
\header {
    title	= "mensural ligature test"
    date	= "2002"
}

\include "paper26.ly"

% Note the horizontal alignment of the fermatas that obeys to the
% graphical width of the ligatures rather to the musical moment in time.
% This is intended behaviour.

voice = \notes \transpose c'' {
  \property Score.timing = ##f
  \property Score.defaultBarType = "empty"
  \property Voice.NoteHead \set #'font-family = #'ancient
  \property Voice.NoteHead \override #'style = #'mensural
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

upperStaff = \context Staff = upperStaff <
  \context MensuralVoice <
    \voice
  >
>

lowerStaff = \context Staff = lowerStaff <
  \context TranscribedVoice <
    \voice
  >
>

\score {
    \context ChoirStaff <
	\upperStaff
	\lowerStaff
    >
    \paper {
	stafflinethickness = \staffspace / 5.0
	\translator {
	    \VoiceContext
	    \name MensuralVoice
	    \alias Voice
	    \remove Ligature_bracket_engraver
	    \consists Mensural_ligature_engraver
	}
	\translator {
	    \VoiceContext
	    \name TranscribedVoice
	    \alias Voice
	    \remove Mensural_ligature_engraver
	    \consists Ligature_bracket_engraver
	}
	\translator {
	    \StaffContext
	    \accepts MensuralVoice
	    \accepts TranscribedVoice
        }
	\translator {
	    \HaraKiriStaffContext
	    \accepts MensuralVoice
	    \accepts TranscribedVoice
        }
    }
}
