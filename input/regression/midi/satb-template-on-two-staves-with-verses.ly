\version "2.27.0"

\header {
  texidoc = "The printed output should have a one-measure SATB score on two
staves with five verses and piano accompaniment.

The MIDI output should include an extra beat of silence where the printed output
has a caesura mark."
}

#(ly:set-option 'warning-as-error #t)

pause = { \caesura \tag "play" s4 } % n.b. not a recommended technique

TwoVoicesPerStaff = ##t

SopranoMusic = \relative { c''4 c \pause c c }
AltoMusic = \relative { g'4 g \pause g g }
VerseOne = \lyricmode { \stanza "1." First _ stan -- za }
VerseTwo = \lyricmode { \stanza "2." Se -- cond stan -- za }
VerseThree = \lyricmode { \stanza "3." Third _ stan -- za }
VerseFour = \lyricmode { \stanza "4." Fourth _ stan -- za }
VerseFive = \lyricmode { \stanza "5." Fifth _ stan -- za }
TenorMusic = \relative { c'4 c \pause c c }
BassMusic = \relative { g2 \pause g4 g }

PianoRHMusic = \relative { c''4 c \pause c c  }
PianoLHMusic = \relative { c2 \pause c }

\layout {
  ragged-right = ##t
}

\include "satb.ly"
