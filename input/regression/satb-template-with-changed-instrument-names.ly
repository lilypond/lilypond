\version "2.19.19"

\header {
  texidoc ="Instrument names and short instrument names
can be changed when using the satb built-in template."
}

Time = { s1 \break s1 }
TwoVoicesPerStaff = ##t

SopranoInstrumentName = "Soprani"
SopranoShortInstrumentName = "Sop"
AltoInstrumentName = "Contralti"
AltoShortInstrumentName = "Con"
MenDividedInstrumentName = "Men Div"
MenDividedShortInstrumentName = "M Div"
MenInstrumentName = "Men Uni"
MenShortInstrumentName = "M Uni"
TenorInstrumentName = "Tenori"
BassInstrumentName = "Bassi"
PianoInstrumentName = "Organ"

MenMusic = \relative { s1 | c4 c c c }
SopranoMusic = \relative { c''4 c c c | c c c c }
AltoMusic = \relative { g'4 g g g | g g g g }
TenorMusic = \relative { c'4 c c c | s1 }
BassMusic = \relative { g2 g4 g | s1 }

PianoRHMusic = \relative { c''4 c c c | c c c c }
PianoLHMusic = \relative { c2 c c c }

\layout {
  ragged-right = ##t
}

\paper { left-margin = 15 }

\include "satb.ly"
