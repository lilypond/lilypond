\version "1.7.16"
\header {
    title	= "vaticana ligature test"
    date	= "2003"
}

\include "paper26.ly"
\include "gregorian-init.ly"

%
% FIXME: custodes and clefs do not show on all staves
% FIXME: some set_char_box() definitions seem to be bad
%

cantus = \notes \relative c {
  \clef "vaticana_fa2"
  \[ f \quilisma g \auctum \descendens a \]
  \[ \virga a g \pes a \inclinatum f \inclinatum d
     c \pes d \quilisma e \pes f \virga g
     a \flexa f \pes g \inclinatum f \inclinatum e \]
  \[ d \quilisma e f \flexa e \pes f \]
  \[ e \flexa d \]
}

verba = \context Lyrics = verba \lyrics {
  Al-4*3 le-4*15 lu-4*5 ia.4*2
}

\score {
  \context VaticanaStaff <
    \context VaticanaVoice <
      \cantus
      \verba
    >
  >
  \paper {
    stafflinethickness = \staffspace / 5.0
    linewidth = 15.0 \cm
%
% FIXME: ragged-right alignment is currently broken
%   width = 15.0 \cm
%   raggedright = ##t
%
    \translator {
      \VoiceContext
      \name VaticanaVoice
      \alias Voice
      \remove Ligature_bracket_engraver
      \consists Vaticana_ligature_engraver
      NoteHead \set #'style = #'vaticana_punctum
      Stem \set #'transparent = ##t
    }
    \translator {
      \StaffContext
      \name VaticanaStaff
      \alias Staff
      \accepts VaticanaVoice
      \remove Bar_engraver
      \consists Custos_engraver
      StaffSymbol \set #'line-count = #4
      TimeSignature \set #'transparent = ##t
      KeySignature \set #'style = #'vaticana
      Accidental \set #'style = #'vaticana
      Custos \set #'style = #'vaticana
      Custos \set #'neutral-position = #3
      Custos \set #'neutral-direction = #-1
      Custos \set #'adjust-if-on-staffline = ##t
    }
    \translator {
      \HaraKiriStaffContext
      \accepts VaticanaVoice
    }
    \translator {
      \ScoreContext
      \accepts VaticanaStaff
      \remove Bar_number_engraver
    }
  }
}
