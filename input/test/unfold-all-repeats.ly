\version "2.7.32"

\header { 
texidoc = "Applying the standard function @code{unfold-repeats} unfolds 
recursively all repeats for a correct MIDI output."
}

mel =  \context Staff {
  \repeat tremolo 8 {c'32 e' }
  \repeat percent 2 { c''8 d'' }
  \repeat volta 2 {c'4 d' e' f'} 
  \alternative {
    { g' a' a' g' }
    {f' e' d' c' }
  }
  \bar "|."
}

\score {  {
  \mel \break
  \applyMusic #unfold-repeats \mel 
 }
}




