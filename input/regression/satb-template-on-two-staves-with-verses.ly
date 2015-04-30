\version "2.19.19"

\header {
  texidoc ="This should produce an SATB score on two staves
with 5 verses and piano accompaniment."

}

TwoVoicesPerStaff = ##t

SopranoMusic = \relative { c''4 c c c }
AltoMusic = \relative { g'4 g g g }
VerseOne = \lyricmode { \set stanza = "1." First _ stan -- za }
VerseTwo = \lyricmode { \set stanza = "2." Se -- cond stan -- za }
VerseThree = \lyricmode { \set stanza = "3." Third _ stan -- za }
VerseFour = \lyricmode { \set stanza = "4." Fourth _ stan -- za }
VerseFive = \lyricmode { \set stanza = "5." Fifth _ stan -- za }
TenorMusic = \relative { c'4 c c c }
BassMusic = \relative { g2 g4 g }

PianoRHMusic = \relative { c''4 c c c  }
PianoLHMusic = \relative { c2 c }

\layout {
  ragged-right = ##t
}

\include "satb.ly"
