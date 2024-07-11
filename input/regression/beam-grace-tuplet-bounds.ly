\version "2.25.19"

\header {
  texidoc = "Tuplets as grace notes should still be subdivided properly, despite their time positions being negative with respect to the main note. A run of grace notes beginning with a tuplet is beamed and bracketed accordingly.  The first three grace notes are a triplet with two beams.  The last grace note is connected to them by one beam."
  texidoc = "A run of grace notes beginning with a tuplet is beamed and bracketed accordingly.  The first three grace notes are a triplet with two beams.  The last grace note is connected to them by one beam."
}

\paper {
  indent = 0
  ragged-right = ##t
}

{
  \time 1/4
  \stopStaff
  \omit Score.BarNumber
  \omit Staff.Clef
  \omit Staff.TimeSignature

  \grace \tuplet 3/2 { \repeat unfold 24 a64 } b4 \break
  \once \set subdivideBeams = ##t
  \grace \tuplet 3/2 { \repeat unfold 24 a64 } b4 \break

  \grace { c8 \tuplet 3/2 { \grace a32 b32 b \tuplet 3/2 { \grace a64 \repeat unfold 6 b64 } \grace a32 b32 b } } b4 \break
  \once \set subdivideBeams = ##t
  \grace { c8 \tuplet 3/2 { \grace a32 b32 b \tuplet 3/2 { \grace a64 \repeat unfold 6 b64 } \grace a32 b32 b } } b4 \break
}
