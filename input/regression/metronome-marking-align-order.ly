\version "2.19.21"

\header {
  texidoc = "Metronome marks respect symbol order in
@code{break-align-symbols}.

In this example, the default is changed to
@code{'(time-signature key-signature)}: since @code{key-signature}
is second in the list, the mark should only be aligned with the key
signature if there is no time signature present, as in the second
measure.
"  
}

\paper {
  ragged-right = ##t
}

\relative {
  \override Staff.KeySignature.break-align-anchor-alignment = #LEFT
  \override Score.MetronomeMark.break-align-symbols = #'(time-signature key-signature)
  \key c \minor
  \tempo "Time"
  c'1 
  \key as \major
  \tempo "Key"
  c1
}
