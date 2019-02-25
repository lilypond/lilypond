\version "2.21.0"

\header {
  texidoc = "Two quoted voices may refer to each other. In this
example, there are notes with each full-bar rest."
}

A = \relative { c'4 d e f | \cueDuring "qB" #1 { R1 } | }
B = \new Voice \relative c' { \cueDuring "qA" #1 { R1 } | f4 e d c | }

\addQuote "qA" \A
\addQuote "qB" \B

<<
  \new Staff \A
  \new Staff \B
>>
