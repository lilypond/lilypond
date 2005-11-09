
\version "2.7.13"

\header{
  texidoc="Beams in unnatural direction, have shortened stems, but do not look too short."
}
\layout{ raggedright = ##t }


\relative c'{
  \override Beam  #'positions =
  #(ly:make-simple-closure
    (ly:make-simple-closure
     (list chain-grob-member-functions
      `(,cons 0 0)
      Beam::calc_least_squares_positions
      Beam::slope_damping
      Beam::shift_region_to_valid
    )))
  
  \stemUp
  f'4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
}

