\version "2.23.7"

\header {
  texidoc = "No lyric repeat count appears at the end of a volta alternative."
}

#(ly:set-option 'warning-as-error #t)

music = \fixed c' {
  c1
  \repeat volta 10 \alternative {
    \volta 1 d1
    \volta 2,3 e1
    \volta 4,5,6 f1
    \volta 7,8,9,10 g1
  }
}

words = \lyricmode {
  C
  \repeat volta 7 \alternative {
    \volta #'(1) D1
    \volta #'(2 3) E1
    \volta #'(4 5 6) F1
    \volta #'(7 8 9 10) G1
  }
}

<<
  \new GregorianTranscriptionStaff \music
  \new GregorianTranscriptionLyrics \words
>>
