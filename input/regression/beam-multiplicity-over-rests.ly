\version "2.15.17"

\header {
  texidoc = "When a beam goes over a rest, beamlets should be as necessary
  to show the beat structure."
}

\relative c' {
  c8[ r16 c32 c32]
  c32[ r16 c32 c8]
  c32[ r16 c64 c64 ~ c16.. c64]
  c32[ c32 r16 c8]
  c16[ r32 c32 r16 c16]
  c16[ r16 c32 r32 c16]
}
