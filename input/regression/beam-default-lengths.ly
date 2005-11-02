\layout{ raggedright = ##t}

\version "2.7.13"

\header{
  texidoc="Beamed stems have standard lengths if possible. Quantization is switched off in this example."
}

\relative c'{
  \override Beam #'position =
  #(ly:make-simple-closure
    (ly:make-simple-closure
     (list chain-grob-member-functions
      `(,cons 0 0)
      Beam::calc_least_squares_positions
      Beam::slope_damping
      Beam::shift_region_to_valid
      Beam::set_stem_lengths
    )))

  f4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
}
