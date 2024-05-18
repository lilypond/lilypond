\version "2.25.17"

\header {
  texidoc = "Kneed beams of strictly monotone subdivisions should have their
  'main beam' be in the middle (outer beamlets of both sides converge
  to/diverge from middle)"
}

\layout {
  indent = #0
  ragged-right = ##t
}

{  
  \stopStaff
  \omit Staff.Clef
  \omit Staff.TimeSignature
  \override Beam.auto-knee-gap = 0

  c64 [32 16 8] s64 c8 [16 32 64] s64 s2 | \break
  c64 [c''''32 c16 c''''8] s64 c8 [c''''16 c32 c''''64] s64
  c''''64 [c32 c''''16 c8] s64 c''''8 [c16 c''''32 c64] s64 | \break
}
