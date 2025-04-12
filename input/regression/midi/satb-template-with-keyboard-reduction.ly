\version "2.25.26"

\header {
  texidoc = "The printed output should have a one-measure SATB score on four
staves with lyrics and a keyboard reduction.

The MIDI output should not include the keyboard reduction.  It should include an
extra beat of silence where the printed output has a caesura mark."
}

#(ly:set-option 'warning-as-error #t)

pause = { \caesura \tag "play" s4 } % n.b. not a recommended technique

SopranoMusic = \relative { c''4 c \pause c c }
SopranoLyrics = \lyricmode { Ah ah ah ah }
AltoMusic = \relative { g'4 g \pause g g }
AltoLyrics = \lyricmode { Lah lah lah lah }
TenorMusic = \relative { c'4 c \pause c c }
TenorLyrics = \lyricmode { Ooo ooo ooo ooo }
BassMusic = \relative { g2 \pause g4 g }
BassLyrics = \lyricmode { Loo loo loo }

PianoInstrumentName = \markup \column { for rehearsal only }
PianoRHMusic = \tag "print" << \SopranoMusic \\ \AltoMusic >>
PianoLHMusic = \tag "print" << \TenorMusic \\ \BassMusic >>

\layout {
  ragged-right = ##t
}

\include "satb.ly"
