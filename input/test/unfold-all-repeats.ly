
\header { 
texidoc = "The standard function unfold-repeats will recursively unfold
all repeats for correct MIDI output. Thanks to Rune Zedeler."
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
%%  \apply #unfold-repeats \mel  FIXME
 }
}


