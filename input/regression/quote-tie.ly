\version "2.12.0"

\header {

  texidoc = " Voices from different cues must not be tied together. In
this example, the first note has a tie. This note should not be tied
to the 2nd note. "

}

\paper {
  ragged-right = ##t
}

cueI = \relative c'' {
  a1 ~ | a | a |
}
\addQuote "cueI" { \cueI }

cueII = \relative c' {
  R1 | e | a |
}
\addQuote "cueII" { \cueII }

\new Staff {
  \cueDuring "cueI" #UP { R1 } |
  R1
  \cueDuring "cueII" #UP { R1 } |
}
