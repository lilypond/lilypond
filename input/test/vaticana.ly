\version "1.9.8"
% possible rename to ancient-something.

\header { texidoc	= "Ancient Vaticana
Vaticana ligature test. "
}

\include "gregorian-init.ly"

cantus = \notes {
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
  \context VaticanaVoice <<
    \cantus
    \verba
  >>
  \paper {
    stafflinethickness = \staffspace / 7.0
    linewidth = 137.0 \mm
    width = 137.0 \mm
    indent = 0.0
    raggedright = ##t
    packed = ##t
    % packed = ##t %%%% FIXME
    \translator {
      \ScoreContext
      \remove Bar_number_engraver
      timing = ##f
      barAlways = ##t
    }
  }
}
