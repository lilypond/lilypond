
\header {

    texidoc = "Two quoted voices may refer to each other. In this
example, there are notes with each full-bar rest."
	  
 
}
\version "2.7.39"

A = \relative c' { c4 d e f | \cueDuring #"qB" #1 { R1 } | }
B = \relative c' { \cueDuring #"qA" #1 { R1 } | f4 e d c  | }

\addquote "qA" \A
\addquote "qB" \B

\layout { ragged-right = ##t }

<<
    \new Staff \A
    \new Staff \B
>>
