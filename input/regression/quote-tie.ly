\version "2.19.21"

\header {
  texidoc = "Voices from different cues must not be tied together.  In
this example, the first note has a tie.  This note should not be tied
to the second visible note (following the rest).
Note that this behavior will not hold for cues in direct succession,
since only one @code{CueVoice} context is created
(with @code{context-id} `cue').
"
}

cueI = \relative {
  a'1 ~ | 1 | a |
}
\addQuote "cueI" { \cueI }

cueII = \relative {
  R1 | e' | a |
}
\addQuote "cueII" { \cueII }

\new Staff \new Voice {
  \cueDuring "cueI" #UP { R1 } |
  R1
  \cueDuring "cueII" #UP { R1 } |
}
