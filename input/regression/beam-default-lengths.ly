\version "2.14.0"

\header {
  texidoc = "Beamed stems have standard lengths if possible.  Quantization
is switched off in this example."
}

\relative c' {
  \override Beam #'skip-quanting = ##t
  f4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f]
}
