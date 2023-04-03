\version "2.25.4"

\header {
  texidoc = "Lyric repeat counts continue to work when lyrics are
applied to music with @code{\\lyricsto}."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

music = \fixed c' {
  \repeat volta 2 { f4 f4 }
  \repeat volta 3 { g2 }
  \repeat volta 4 { a1 }
  \repeat volta 1 b\breve
  c'\breve
}

words = \lyricmode {
  \repeat volta 2 { F -- f }
  \repeat volta 3 G
  \repeat volta 4 A
  \repeat volta 1 B
  C
}

piece = <<
  \new GregorianTranscriptionStaff = "S" \music
  \new GregorianTranscriptionLyrics \lyricsto Staff = "S" \words
>>

\score { \piece }
