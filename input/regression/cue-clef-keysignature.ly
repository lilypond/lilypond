\version "2.15.3"

\header {
  texidoc = "Clefs for cue notes should not influence the printed key signature."
}

vI = \relative c'' { \clef "treble" \repeat unfold 40 g4 }
\addQuote vIQuote { \vI }
\score {
  \new Staff {
    \clef "bass" \key g \major
    \cueDuringWithClef #"vIQuote" #DOWN #"treble" { R1 } |
    c1 |
    \cueDuringWithClef #"vIQuote" #DOWN #"soprano" { R1 \break R1 }
    c1 |
  }
}
