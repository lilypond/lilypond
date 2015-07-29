\version "2.19.25"

\header {
  texidoc ="Instrument names and short instrument names
can be changed when using the ssaattbb built-in template."
}

Time = { s1 \break s1 }
TwoVoicesPerStaff = ##t

SopranoOneInstrumentName = "Sop One"
SopranoOneShortInstrumentName = "Sop 1"
SopranoTwoInstrumentName = "Sop Two"
SopranoTwoShortInstrumentName = "Sop 2"
MenDividedInstrumentName = "Men Div"
MenDividedShortInstrumentName = "M Div"
MenInstrumentName = "Men Uni"
MenShortInstrumentName = "M Uni"
TenorInstrumentName = "Tenori"
BassInstrumentName = "Bassi"
PianoInstrumentName = "Organ"

MenMusic = \relative { s1 | c4 c c c }
SopranoOneMusic = \relative { c''4 c c c | c c c c }
SopranoTwoMusic = \relative { g'4 g g g | g g g g }
TenorMusic = \relative { c'4 c c c | s1 }
BassMusic = \relative { g2 g4 g | s1 }

PianoRHMusic = \relative { c''4 c c c | c c c c }
PianoLHMusic = \relative { c2 c c c }

\layout {
  ragged-right = ##t
}

\paper { left-margin = 15 }

\include "ssaattbb.ly"
