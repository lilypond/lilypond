
\version "2.12.0"

\header{
  texidoc="Beams in unnatural direction, have shortened stems, but do not look too short."
}
\layout{ ragged-right = ##t }


\relative c'{
  \override Beam  #'positions =
  #(ly:make-simple-closure
    (ly:make-simple-closure
     (list chain-grob-member-functions
      `(,cons 0 0)
      ly:beam::calc-least-squares-positions
      ly:beam::slope-damping
      ly:beam::shift-region-to-valid
    )))
  
  \stemUp
  f'4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
}

