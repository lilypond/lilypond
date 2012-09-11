\version "2.16.0"

\header {
  texidoc = "Pitched trills on consecutive notes with the same
name and octave should not lose accidentals; in the following
example, accidentals should be visible for all trill-pitches.
"
}

\relative c' {
  \pitchedTrill
  f4\startTrillSpan ges f\stopTrillSpan
  
  \pitchedTrill
  f4\startTrillSpan g gis\stopTrillSpan ~
  
  \pitchedTrill
  gis4\startTrillSpan ges f\stopTrillSpan
  
  \pitchedTrill
  g4\startTrillSpan gis f\stopTrillSpan
  
  \pitchedTrill
  f4\startTrillSpan gisis f\stopTrillSpan
  
  \pitchedTrill
  f4\startTrillSpan geses f\stopTrillSpan
}
