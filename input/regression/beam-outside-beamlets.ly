\header {
  texidoc = "Beams may overshoot stems. This is also controlled with @code{break-overshoot}."

}

\version "2.7.13"
\paper {
  raggedright = ##t
}



{
  c8[
    c16
    c]

  % only works for the right edge, sorry. 
  \override Beam #'break-overshoot = #'(-0.5 . 0.5) 
  \set stemLeftBeamCount = #2
  c8[
    c16
    \set stemRightBeamCount = #3
    c ]  
}
