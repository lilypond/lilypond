\version "1.7.19"
\header {
    title	= "Gregorian Scripts"
    texidoc     = "@cindex Gregorian Scripts

Gregorian Scripts:

ictus, circulus, semicirculus, accentus, episem.

TODO: augmentum.

FIXME: when applying an episem within a ligature, the TextSpanner's width
collapses to 0.0.

FIXME: clef does not show on each line

FIXME: horizontal spacing (ragged right mode)

FIXME: padding/minimum-distance is fragile.

"}

\include "gregorian-init.ly"

cantus = \notes \relative c' {
  \clef "vaticana_do2"

  a-\ictus
  a-\circulus
  a-\semicirculus
  a-\accentus

  %{ %% TODO: augmentum:
     a-\augmentum
     \[ \augmentumInitium b \flexa a \augmentumFinis \]
  %}

  a \episemInitium b \flexa a \episemFinis

  \[ a \episemInitium b \flexa a \episemFinis \]
}

\score {
  \context VaticanaStaff <
    \context VaticanaVoice <
      \cantus
    >
  >
  \paper {
    stafflinethickness = \staffspace / 5.0
    linewidth = 70.0
    width = 60.0
    indent = 0.0
    raggedright = ##t

%   width = 15.0 \cm %%% no effect?
%   gourlay_maxmeasures = 1. %%% no effect?

    \translator {
      \VoiceContext
      \name VaticanaVoice
      \alias Voice
      \remove "Stem_engraver"
      \remove Ligature_bracket_engraver
      \consists Vaticana_ligature_engraver
      NoteHead \set #'style = #'vaticana_punctum
      Script \set #'padding = #0.0

      % prepare TextSpanner for \episem{Initium|Finis} use
      TextSpanner \set #'type = #'line
      TextSpanner \set #'edge-height = #'(0 . 0)
      TextSpanner \set #'padding = #0.5
      TextSpanner \set #'edge-text = #'("" . "")
    }
    \translator {
      \StaffContext
      \name VaticanaStaff
      \alias Staff
      \accepts VaticanaVoice
      \remove Bar_engraver
      \consists Custos_engraver
      StaffSymbol \set #'line-count = #4
%      StaffSymbol \set #'width = #60.0 % FIXME: should be \linewidth
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
