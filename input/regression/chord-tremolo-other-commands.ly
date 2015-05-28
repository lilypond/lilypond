\version "2.19.21"

\header {
  texidoc = "
To calculate the total duration of chord tremolos, only real notes shall be 
counted, no other commands.
"
}

right = \relative c'' {
  s2
}

left = \relative {
  % This tremolo contains just two notes (but three lilypond events/commands!)
  \repeat tremolo 4 { f16 \change Staff = "right" f'} 
}

\score {
  \new PianoStaff <<
    \new Staff = "right" { \right }
    \new Staff = "left" { \clef bass \left }
  >>
}
