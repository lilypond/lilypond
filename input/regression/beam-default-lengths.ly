\layout{ ragged-right = ##t}

\version "2.12.0"

\header{
  texidoc="Beamed stems have standard lengths if possible. Quantization is switched off in this example."
}

\relative c'{
  \override Beam #'position =
  #(ly:make-simple-closure
    (ly:make-simple-closure
     (list chain-grob-member-functions
      `(,cons 0 0)
      ly:beam::calc-least-squares-positions
      ly:beam::slope-damping
      ly:beam::shift-region-to-valid
      ly:beam::set-stem-lengths
    )))

  f4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
}
