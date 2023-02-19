\version "2.27.6"

\header {
  texidoc = "Commands affecting tempo indications are collected score-wide."
}

\layout { ragged-right = ##t }

#(ly:set-option 'warning-as-error #t)

<<
  \new Staff {
     \override Score.TempoSpanner.to-barline = ##f
     b1 1 1 1
  }
  \new Staff {
     \tempo 4=120
     \tempoSpan
     b1 1 1 1
  }
  \new Staff {
     b1
     \tempoMark "allargando"
     \tempoSpan
     1 1 1
     \tempo 4=80
     \bar "|."
  }
>>
