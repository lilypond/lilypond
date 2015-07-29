\version "2.19.25"

\header {
  texidoc ="This should produce a choral score with solo,
descant, women, sop 1 and 2, sop, alto, alto 1 and 2,
tenor 1 and 2, tenor, bass, bass 1 and 2, men and piano
staves.  Normally the various combinations would appear
at different times in the score, not all at once. "
}

SoloMusic = \relative { e''4 e e e | }
SoloLyrics = \lyricmode { So -- lo ly -- rics }
DescantMusic = \relative { e''4 e e e | }
DescantLyrics = \lyricmode { Des -- cant ly -- rics }
WomenMusic = \relative {g'4 g g g }
WomenLyrics = \lyricmode { Wo -- men ly -- rics }
SopranoOneMusic = \relative { c''8 e c e e4 e | }
SopranoOneLyrics = \lyricmode { So -- pra -- no One ly -- rics }
SopranoTwoMusic = \relative { c''8 c c c c4 c | }
SopranoTwoLyrics = \lyricmode { So -- pra -- no Two ly -- rics }
SopranoMusic = \relative { c''4 c c c8 c }
SopranoLyrics = \lyricmode { So -- pra -- no ly -- rics }
AltoOneMusic = \relative { g'16 g g( c) g2 g4 }
AltoOneLyrics = \lyricmode { Al -- to One ly -- rics }
AltoTwoMusic = \relative { g'16 e e c g'2 }
AltoTwoLyrics = \lyricmode { Al -- to Two ly -- rics }
AltoMusic = \relative { g'4 g g g | }
AltoLyrics = \lyricmode { Al -- to ly -- rics }
TenorOneMusic = \relative { c'8 c c4 c c | }
TenorOneLyrics = \lyricmode { Te -- nor One ly -- rics }
TenorTwoMusic = \relative { c'8 c g4 g g | }
TenorTwoLyrics = \lyricmode { Te -- nor Two ly -- rics }
TenorMusic = \relative { c'4 c c c | }
TenorLyrics = \lyricmode { Te -- nor ly -- rics }
BassMusic = \relative { g2 g4 g | }
BassLyrics = \lyricmode { Bass ly -- rics }
BassOneMusic = \relative { g4 g g g | }
BassOneLyrics = \lyricmode { Bass One ly -- rics }
BassTwoMusic = \relative { g4 g g g | }
BassTwoLyrics = \lyricmode { Bass Two ly -- rics }
MenMusic = \relative { e2 e4 e | }
MenLyrics = \lyricmode { Men ly -- rics }

PianoRHMusic = \relative { c''4 c c c | }
PianoLHMusic = \relative { c2 c | }

\layout {
  ragged-right = ##t
}

\include "ssaattbb.ly"
