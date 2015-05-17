\version "2.19.21"

\header {
  texidoc = "When a beam goes over a rest, beamlets should be as necessary
  to show the beat structure."
}

\relative {
  c'8[ r16 c32 c32]
  c32[ r16 c32 c8]
  c32[ r16 c64 c64 ~ 16.. c64]
  c32[ c32 r16 c8]
  c16[ r32 c32 r16 c16]
  c16[ r16 c32 r32 c16]
}
