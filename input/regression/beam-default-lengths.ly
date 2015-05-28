\version "2.19.21"

\header {
  texidoc = "Beamed stems have standard lengths if possible.  Quantization
is switched off in this example."
}

\relative {
  \override Beam.skip-quanting = ##t
  f'4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f]
}
