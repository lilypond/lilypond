\version "2.3.4"

\header { texidoc	= "@cindex Ancient Vaticana
Vaticana ligature uses four staff lines, special clef, an notes
calligraphic notes. "
}

\include "gregorian-init.ly"

cantus = \context VaticanaVoice = "cantus"  {
  \clef "vaticana_fa2"
  \[ f\melisma \quilisma g \auctum \descendens a\melismaEnd \]
  \[ \virga a\melisma g \pes a \inclinatum f \inclinatum d
     c \pes d \quilisma e \pes f \virga g
     a \flexa f \pes g \inclinatum f \inclinatum e\melismaEnd \]
  \[ d\melisma \quilisma e f \flexa e \pes f\melismaEnd \]
  \[ e\melisma \flexa d\melismaEnd \]
}

verba = \context Lyrics = "verba" \lyrics {
  Al- le- lu- ia.
}

\score {
  <<
    \cantus
    \lyricsto "cantus" \verba
  >>
  \paper {
    linethickness = \staffspace / 7.0
    linewidth = 137.0 \mm
    width = 137.0 \mm
    indent = 0.0
    raggedright = ##t
    packed = ##t
    \context {
      \Score
      \remove Bar_number_engraver
      timing = ##f
      barAlways = ##t
    }
  }
}
