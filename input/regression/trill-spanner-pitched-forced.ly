\version "2.12.0"

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
