\version "2.10.0"

\header{
texidoc="Chord tremolos on a single note."
}

\context Voice \relative c' {
  \time 4/4
  \repeat "tremolo" 32 { d32 }

  c4 c4 c4 c4 c4 
}

