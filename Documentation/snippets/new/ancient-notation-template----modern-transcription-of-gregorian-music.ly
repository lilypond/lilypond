\version "2.17.30"

\header {
  lsrtags = "vocal-music, ancient-notation, template"

  texidoc = "
This example demonstrates how to do modern transcription of Gregorian
music. Gregorian music has no measure, no stems; it uses only half and
quarter note heads, and special marks, indicating rests of different
length.

"
  doctitle = "Ancient notation template -- modern transcription of gregorian music"
}

\include "gregorian.ly"

chant = \relative c' {
  \set Score.timing = ##f
  f4 a2 \divisioMinima
  g4 b a2 f2 \divisioMaior
  g4( f) f( g) a2 \finalis
}

verba = \lyricmode {
  Lo -- rem ip -- sum do -- lor sit a -- met
}

\score {
  \new Staff <<
    \new Voice = "melody" \chant
    \new Lyrics = "one" \lyricsto melody \verba
  >>
  \layout {
    \context {
      \Staff
      \remove "Time_signature_engraver"
      \remove "Bar_engraver"
      \hide Stem
    }
    \context {
      \Voice
      \override Stem.length = #0
    }
    \context {
      \Score
      barAlways = ##t
    }
  }
}

