\version "2.19.25"

\header {
  texidoc ="This should produce an SSAATTBB score with piano
accompaniment, with divisi soprano and tenor on single staves,
alto one and alto two on separate staves and unison bass in the
first system, then unison soprano and alto voices with descant
in the second system and unison women and unison men voices in
the third system. "
}

SopranoTwoVoicesPerStaff = ##t
TenorTwoVoicesPerStaff = ##t

Time = { s1 \break | s1 \break }

DescantMusic = \relative { s1 | e''4 e e e | s1 }
DescantLyrics = \lyricmode { Des -- cant ly -- rics }
SopranoOneMusic = \relative { c''8 e c e e4 e | s1*2 | }
SopranoOneLyrics = \lyricmode { So -- pra -- no One ly -- rics }
SopranoTwoMusic = \relative { c''8 c c c c4 c | s1*2 | }
SopranoTwoLyrics = \lyricmode { So -- pra -- no Two ly -- rics }
SopranoMusic = \relative { s1 | c''4 c c c8 c }
SopranoLyrics = \lyricmode { So -- pra -- no ly -- rics }
AltoOneMusic = \relative { g'16 g g( c) g2 g4 | s1*2 | }
AltoOneLyrics = \lyricmode { Al -- to One ly -- rics }
AltoTwoMusic = \relative { r4 g'16 e e c g'2 | s1*2 | }
AltoTwoLyrics = \lyricmode { Al -- to Two ly -- rics }
AltoMusic = \relative { s1 | g'4 g g g | }
AltoLyrics = \lyricmode { Al -- to ly -- rics }
WomenMusic = \relative {s1*2 | g4 g g g }
WomenLyrics = \lyricmode { Wo -- men ly -- rics }
MenMusic = \relative { s1*2 | e2 e4 e | }
MenLyrics = \lyricmode { Men ly -- rics }
TenorOneMusic = \relative { c'8 c c4 c c | s1*2 | }
TenorOneLyrics = \lyricmode { Te -- nor One ly -- rics }
TenorTwoMusic = \relative { c'8 c g4 g g | s1*2 | }
TenorTwoLyrics = \lyricmode { Te -- nor Two ly -- rics }
BassMusic = \relative { g2 g4 g | s1*2 }
BassLyrics = \lyricmode { Bass ly -- rics }

PianoRHMusic = \relative { c''4 c c c | c c c c | c c c c | }
PianoLHMusic = \relative { c2 c | c c | c c | }

\layout {
  ragged-right = ##t
}

\include "ssaattbb.ly"
