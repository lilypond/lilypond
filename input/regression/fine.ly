\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\fine} places a performance instruction below all
staves and at end-of-line at a break, except at the written end of the
music.  The context property @code{fineText} controls the text."
}

\layout {
  ragged-right = ##t

  \context {
    \Score
    fineText = \markup \column { "FI-" "NE" }
  }
}

staff = \new Staff \fixed c' {
  \repeat volta 2 {
    f1
    \volta 2 \fine \break
    b1
  }
  \fine
}

\new PianoStaff << \staff \staff >>
