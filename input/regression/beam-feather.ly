\header  {

  texidoc = "Specifying @code{grow-direction} on a beam, will cause
  feathered beaming. The @code{\featherDurations} function can be used
  to adjust note durations."
  
}

\version "2.9.4"

\paper {
  ragged-right = ##t
  indent = #0.0
}

\featherDurations #(ly:make-moment 3 4) \relative c'' {
  \override Beam #'grow-direction = #LEFT
  c16[
    c c c
    c c c c ] 
}
