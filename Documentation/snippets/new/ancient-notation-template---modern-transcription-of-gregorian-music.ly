\version "2.25.5"

\header {
  categories = "Ancient notation, Template, Vocal music"

  texidoc = "
This example demonstrates how to do modern transcription of Gregorian
music. Gregorian music has no measure, no stems; it uses only half and
quarter note heads, and special marks, indicating rests of different
length.
"

  doctitle = "Ancient notation template – modern transcription of Gregorian music"
}


chant = \relative c' {
  \set Score.timing = ##f
  f4 a2 \divisioMinima
  g4 b a2 f2 \divisioMaior
  g4( f) f( g f) a2 \finalis \break
  f4 a2 \divisioMinima
  g4 b a2 f2 \divisioMaior
  g4( f) f( g a) g2( f) \finalis
}

verba = \lyricmode {
  Lo -- rem ip -- sum do -- lor sit a -- met,
  lo -- rem ip -- sum do -- lor sit a -- met.
}

\score {
  \new GregorianTranscriptionStaff <<
    \new GregorianTranscriptionVoice = "melody" \chant
    \new GregorianTranscriptionLyrics = "one" \lyricsto melody \verba
  >>
}
