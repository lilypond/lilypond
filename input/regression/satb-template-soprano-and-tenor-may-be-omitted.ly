\version "2.19.19"

\header {
  texidoc ="Soprano and tenor voices may be omitted without
error, even when TwoVoicesPerStaff is specified and Alto
and Bass lyrics are provided."
}
TwoVoicesPerStaff = ##t

AltoMusic = \relative { g'4 g g g }
AltoLyrics = \lyricmode { Al -- to ly -- rics }
BassMusic = \relative { g2 g4 g }
BassLyrics = \lyricmode { Bass ly -- rics }

\layout {
  ragged-right = ##t
}

\include "satb.ly"
