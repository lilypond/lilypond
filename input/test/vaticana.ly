\version "1.7.19"
\header {
    texidoc	= "vaticana ligature test"
}

\include "paper26.ly"
\include "gregorian-init.ly"

%
% FIXME: custodes and clefs do not show on all staves
%

%
% FIXME: move VaticnaStaff/VaticanaVoice definition to engraver-init.ly?
% Or rather to gregorian-init.ly?
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
    indent = 0.0
%
% FIXME: ragged-right alignment is currently broken
%   width = 12.0 \cm
%   raggedright = ##t
%   gourlay_maxmeasures = 30.
%
    \translator {
      \VoiceContext
      \name VaticanaVoice
      \alias Voice
      \remove "Stem_engraver"
      \remove Ligature_bracket_engraver
      \consists Vaticana_ligature_engraver
      NoteHead \set #'style = #'vaticana_punctum
%     TextScript \set #'padding = #0.0
    }
    \translator {
      \StaffContext
      \name VaticanaStaff
      \alias Staff
      \accepts VaticanaVoice
      \remove Bar_engraver
      \consists Custos_engraver
      StaffSymbol \set #'line-count = #4
%     StaffSymbol \set #'width = #60.0 % FIXME: how to get same as \linewidth?
      TimeSignature \set #'transparent = ##t
      KeySignature \set #'style = #'vaticana
      Accidental \set #'style = #'vaticana
      Custos \set #'style = #'vaticana
      Custos \set #'neutral-position = #3
      Custos \set #'neutral-direction = #-1
      Custos \set #'adjust-if-on-staffline = ##t
    }
    \translator {
      \RemoveEmptyStaffContext
      \accepts VaticanaVoice
    }
    \translator {
      \ScoreContext
      \accepts VaticanaStaff
      \remove Bar_number_engraver
    }
  }
}
