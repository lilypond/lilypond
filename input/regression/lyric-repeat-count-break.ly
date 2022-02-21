\version "2.23.7"

\header {
  texidoc = "At a line break, a lyric repeat count is visible at the
end of the line."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

music = \fixed c' {
  \repeat volta 2 { f1 }
  \break
  a1
}

words = \lyricmode {
  \repeat volta 2 F1
  A1
}

piece = <<
  \new GregorianTranscriptionStaff \music
  \new GregorianTranscriptionLyrics \words
>>

\score { \piece }
