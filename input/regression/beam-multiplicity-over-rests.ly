\version "2.12.0"

\header {
  texidoc = "When a beam goes over a rest, there should not be any 
beamlets pointing towards the rest unless absolutely necessary."
}

\relative c' {
  c8[ r16 c32 c32]
  c32[ r16 c32 c8]
  c32[ r16 c64 c8 c64]
  c32[ c32 r16 c8]
  c16[ r32 c32 r16 c16]
  c16[ r16 c32 r32 c16]
}