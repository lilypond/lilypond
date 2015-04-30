\version "2.19.19"

\header {
  texidoc ="This should produce an SATB score with piano
accompaniment, with four voices in the first system, unison
women voices with descant in the second system and unison
women and unison men voices in the third system. "
}

Time = { s1 \break s1 \break }

DescantMusic = \relative { s1 | e''4 e e e | s1 }
DescantLyrics = \lyricmode { Des -- cant ly -- rics }
SopranoMusic = \relative { c''8 c c4 c c | s1*2 | }
SopranoLyrics = \lyricmode { So -- pra -- no ly -- rics }
AltoMusic = \relative { g'4 g g g | s1*2 | }
AltoLyrics = \lyricmode { Al -- to ly -- rics }
WomenMusic = \relative { s1 | g'4 g g g | g g g g | }
WomenLyrics = \lyricmode { Wo -- men ly -- rics Wo -- men ly -- rics }
MenMusic = \relative { s1*2 | e2 e4 e | }
MenLyrics = \lyricmode { Men ly -- rics }
TenorMusic = \relative { c'4 c c c | s1*2 | }
TenorLyrics = \lyricmode { Te -- nor ly -- rics }
BassMusic = \relative { g2 g4 g | s1*2 }
BassLyrics = \lyricmode { Bass ly -- rics }

PianoRHMusic = \relative { c''4 c c c | c c c c | c c c c | }
PianoLHMusic = \relative { c2 c | c c | c c | }

\layout {
  ragged-right = ##t
}

\include "satb.ly"
