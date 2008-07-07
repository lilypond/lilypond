\version "2.11.51"

\header {
  texidoc = "Pitched trill accidentals can be forced."
}

\relative c' {  
  \pitchedTrill
  c4\startTrillSpan es f\stopTrillSpan
  \pitchedTrill
  c4\startTrillSpan es f\stopTrillSpan
  \pitchedTrill
  c4\startTrillSpan es f\stopTrillSpan
  \pitchedTrill
  c4\startTrillSpan-"forced" es! f\stopTrillSpan
}
