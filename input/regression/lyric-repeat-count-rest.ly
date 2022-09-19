\version "2.23.14"

\header {
  texidoc = "A lyric repeat count is placed at the end of a repeated
section even when that occurs during a rest.  In this test, an arrow
marks the expected position of the repeat count."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

music = \fixed c' {
  \repeat volta 2 { r1 f1 r1 }
  \tweak self-alignment-X #CENTER \textMark "↓"
  r1 a1
}

words = \lyricmode {
  \repeat volta 2 { \skip 1 F1 \skip 1 }
  \skip 1 A1
}

piece = <<
  \new GregorianTranscriptionStaff \music
  \new GregorianTranscriptionLyrics \words
>>

\score { \piece }
