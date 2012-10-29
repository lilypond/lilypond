\version "2.17.6"

\header {
  texidoc = "@code{\\tempo} marks are aligned with the time signature
or first musical element unless the first element is a multi-measure
rest: in this case, the tempo mark is aligned with the bar line.

By overriding @code{break-align-symbols} the default alignment can be
changed, as shown by the final metronome mark in this snippet, aligned
with a key signature.
"  
}

\layout {
  line-width = 50\mm
}

\relative c' {
  \tempo "T-first"
  c1
  \mark \default
  \tempo "T-note"
  c1 \break
  \tempo "T-break"
  c1
  \tempo "T-rest"
  R1 \break
  \time 8/8
  \tempo "T-time"
  R1
  \override Score.MetronomeMark.break-align-symbols = #'(key-signature)
  \key as \major
  \tempo "T-key"
  R1
}
