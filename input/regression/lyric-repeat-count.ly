\version "2.25.4"

\header {
  texidoc = "This tests the appearance of repeats for modern
transcriptions of Gregorian chant.  The repeat count appears in the
lyric line under the finalis sign (double line) that ends the repeated
section, even if the repeat count is@tie{}1.  The count is an italicized
lowercase roman number followed by a period.  A final ``i'' is
replaced by@tie{}``j''."
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
  \repeat volta 2 { F4 -- f4 }
  \repeat volta 3 G2
  \repeat volta 4 A1
  \repeat volta 1 B\breve
  C\breve
}

piece = <<
  \new GregorianTranscriptionStaff \music
  \new GregorianTranscriptionLyrics \words
>>

\score {
  \layout {
    \context {
      \GregorianTranscriptionStaff
      instrumentName = "default"
    }
  }
  \piece
}

\score {
  \layout {
    \context {
      \GregorianTranscriptionStaff
      instrumentName = "Divisio"
      \EnableGregorianDivisiones
    }
  }
  \piece
}

\score {
  \layout {
    \context {
      \GregorianTranscriptionStaff
      instrumentName = "unfolded"
      \EnableGregorianDivisiones
    }
  }
  { \unfoldRepeats \piece }
}
