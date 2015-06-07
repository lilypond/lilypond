\version "2.19.21"

\header {
  texidoc = "A tremolo can have more than two notes. Also check that
linebreaks between tremolos still work and that empty tremolos don't crash."
}

\paper { ragged-right = ##t }
\score {
  \new Staff \relative {
    \time 3/4
    \repeat tremolo 16 { a64 c e } |
    \repeat tremolo 8 { a,64 c e }
    \repeat tremolo 4 { a,64 c e }
    \repeat tremolo 2 { a,32 c e } |\break
    \repeat tremolo 16 { a64 c e }
    \repeat tremolo 8 {} |
  }
}
