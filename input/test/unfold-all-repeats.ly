\version "2.2.0"
% regression.  -gp

\header { 
texidoc = "Applying the standard function @code{unfold-repeats} unfolds 
recursively all repeats for a correct MIDI output."
}


mel = \notes \context Staff {
  \repeat tremolo 8 {c'32 e' }
  \repeat percent 2 { c''8 d'' }
  \repeat volta 2 {c'4 d' e' f'} 
  \alternative {
    { g' a' a' g' }
    {f' e' d' c' }
  }
  \bar "|."
}

\score { \notes {
  \mel \break
  \apply #unfold-repeats \mel 
 }
}



