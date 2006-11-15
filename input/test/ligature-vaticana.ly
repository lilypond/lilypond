\version "2.10.0"

\header { texidoc	= "@cindex Ancient Vaticana
Vaticana ligature uses four staff lines, special clef, and
calligraphic notes.  Augmentum dots are collected behind
ligatures. "
}

\include "gregorian-init.ly"

cantus = \new VaticanaVoice = "cantus"  {
  \clef "vaticana-fa2"
  \[ f\melisma \quilisma g \auctum \descendens a\melismaEnd \]
  \[ \virga a\melisma g \pes a \inclinatum f \inclinatum d
     c \pes d \quilisma e \pes f \virga g
     a \flexa f \pes g \inclinatum f \inclinatum e\melismaEnd \]
  \[ d\melisma \quilisma e f \flexa e \pes f\melismaEnd \]
  \[ \augmentum e\melisma \flexa \augmentum d\melismaEnd \]
}

verba = \new Lyrics = "verba" \lyricmode {
  Al- le- lu- ia.
}

\paper {
    line-thickness = \staff-space / 7.0
}

\score {
  <<
    \cantus
    \lyricsto "cantus" \verba
  >>
  \layout {
    line-width = 137.0 \mm
    width = 137.0 \mm
    indent = 0.0
    ragged-right = ##t
    packed = ##t
    \context {
      \Score
      \remove Bar_number_engraver
      timing = ##f
      barAlways = ##t
    }
  }
}
